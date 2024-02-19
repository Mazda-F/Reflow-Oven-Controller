import numpy as np
import matplotlib.pyplot as plt
import sys
import time
import serial
import pyttsx3
import smtplib
from email.mime.text import MIMEText

# Function to announce the current state using text-to-speech
def announce_state(state):
    engine = pyttsx3.init()
    engine.say(f"Entering {state} stage")
    engine.runAndWait()

#User input for the preheat & reflow temperature
preheat_temp = float(input("Enter the preheat temperature: "))
reflow_temp = float(input("Enter the reflow temperature: "))

# Set the initial state
preheat_state = False
reflow_state = False
cooling_state = False

# Configure the serial port
ser = serial.Serial(
    port='COM8',
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    bytesize=serial.EIGHTBITS
)

# Open the serial port
ser.isOpen()

# Initialize variables for plotting and storing data
t = 0
data_values = []
data_file = 'temperature_data.txt'  # File to store temperature readings

# Create a figure and axis for plotting
fig, ax = plt.subplots()

try:
    while True:
        # Read the data from the serial port
        data = ser.readline().decode().strip()

        print("Received data:", data)  # Debugging line

        try:
            # Convert the received data to a float
            temperature = float(data)

            # Convert temperature to Fahrenheit and Kelvin
            temperature_fahrenheit = temperature * 9/5 + 32
            temperature_kelvin = temperature + 273.15

            print("Converted temperature:", temperature)  # Debugging line

            # Add the new point to the data_values list
            data_values.append(temperature)


            print("Preheat State:", preheat_state)
            print("Reflow State:", reflow_state)
            print("Cooling State:", cooling_state)
            print("\n")


            # Determine the current state of the reflow process
            if not preheat_state and temperature >= preheat_temp-7 and temperature <= preheat_temp+7:
                preheat_state = True
                cooling_state = False
                reflow_state = False
                announce_state("Preheat")
            elif not reflow_state and preheat_state and temperature >= reflow_temp and temperature <= 255:
                reflow_state = True
                announce_state("Reflow")
            elif not cooling_state and reflow_state and preheat_state and temperature <= reflow_temp:
                #SEND EMAIL THAT THE REFLOW PROCESS IS DONE AND PCB IS READY
                cooling_state = True  
                preheat_state = False
                # SEND EMAIL THAT THE REFLOW PROCESS IS DONE AND PCB IS READY
                recipient_email = "recipient.email@gmail.com"  # Recipient's email address
                subject = "PCB Reflow Process Completed"
                body = "Your PCB reflow process is complete, and the PCB is ready for further handling."

                # Compose the email message
                message = MIMEText(body)
                message["Subject"] = subject
                message["From"] = "reflowoven@gmail.com"  
                message["To"] = recipient_email

                # Connect to the SMTP server and send the email
                smtp_server = "smtp.gmail.com"  
                smtp_port = 587  
                smtp_username = "reflowoven@gmail.com"  
                smtp_password = "reflowovenpassword" 

                try:
                    with smtplib.SMTP(smtp_server, smtp_port) as server:
                        server.starttls()
                        server.login(smtp_username, smtp_password)
                        server.sendmail(message["From"], [message["To"]], message.as_string())
                    print("Email sent successfully.")
                except Exception as e:
                    print(f"Error sending email: {e}")


            #Update the plot
            ax.clear()
            ax.plot(range(len(data_values)), data_values)
            ax.set_xlabel('Time(seconds)')
            ax.set_ylabel('Temperature(celsius)')
            ax.set_title('Temperature Plot')

            # Display Fahrenheit and Kelvin temperatures on the chart
            ax.annotate(f'{temperature_fahrenheit:.2f} F', xy=(0.02, 0.95), xycoords='axes fraction', fontsize=10, color='red')
            ax.annotate(f'{temperature_kelvin:.2f} K', xy=(0.02, 0.90), xycoords='axes fraction', fontsize=10, color='blue')

            plt.pause(0.01)
            
            # Store temperature reading in a file
            with open(data_file, 'a') as file:
                file.write(f"{time.time()},{temperature}\n")

        except ValueError:
            print("Invalid data received:", data)
            # Go to the intial ramp state if the data is invalid(microcontroller reset)
            preheat_state = False
            reflow_state = False
            cooling_state = False

        t += 1
        # Pause for one second
        time.sleep(1)

except KeyboardInterrupt:
    print("Script interrupted. Closing serial port and exiting.")
    ser.close()
    sys.exit(0)
