import os

# Specify the folder path where your .txt files are located
folder_path = '.03_Datasets/99_label_files/labels'

# Specify the path to the names.txt file
names_file_path = './class_names.txt'


# Specify the output file where the concatenated content will be saved
output_file_path = './summaryVideos.csv'

# Read names from names.txt file
with open(names_file_path, 'r') as names_file:
    # Create a list of names from the file
    names_list = [name.strip() for name in names_file.readlines()]

# Open the output file in write mode
with open(output_file_path, 'w') as output_file:
    # Iterate over each file in the folder
    for filename in os.listdir(folder_path):
        # Check if the file has a .txt extension
        if filename.endswith('.txt'):
            # Get the full path of the current file
            file_path = os.path.join(folder_path, filename)
            
            # Open the current file in read mode
            with open(file_path, 'r') as current_file:
                # Iterate over each line in the current file
                for line in current_file:
                    # Split the line into columns using whitespaces
                    columns = line.split()
                    
                    # Check if the line has enough columns
                    if len(columns) >= 2:
                        # Get the number from the second column
                        number = int(columns[0].strip())
                        
                        if number == 99:
                            columns[0] = 'NA'
                        else:
                            # Replace the number with the corresponding name from names_list
                            if 0 <= number <= len(names_list) - 1:
                                columns[0] = names_list[number]
                    

                        
                    # Write the modified line to the output file
                    output_file.write(f"{filename} {' '.join(columns)}\n")

# Print a message indicating that the operation is complete
print("Concatenation, prefixing, and number replacement completed.")