import os 

def markdown_link(title):
    """Generate valid markdown link from title"""
    link = title.strip()
    link = link.replace(" ", "-")
    link = link.lower()
    link = "".join(e for e in link if e.isalnum() or e == "-")
    # link = "-".join([part for part in link.split("-") if part])
    return f"[{title.strip()}](#{link})"

# Get all Fortran source files
source = os.path.join(os.path.abspath(os.path.curdir), "src")
files = [file for file in os.listdir(source) if file.endswith((".f90", ".F90"))]

lines = list()
for file in files:
    # Extract all documentation lines
    with open(os.path.join(source, file), "r") as handle:
        in_block = False
        for line in handle:
            if line.find(r'%%%') > 0:
                in_block = not in_block
            elif in_block:
                # Process all lines:
                # * Strip Fortran comment symbol (!)
                # * Add trailing spaces for MD formatting
                i = line.find("!")
                line = line[i+2:].rstrip()
                if line.startswith("*"):
                    line += "</br>"
                lines.append(line)
    # Add line break after each section
    lines.append("-"*80)
    
# Generate TOC from lines
toc = ["# Table of Contents"]
for line in lines:
    # Skip Fortran macros
    if line.startswith("#define"):
        continue
    elif line.startswith("###"):
        continue
    elif line.startswith("##"):
        link = "  " + "* " + markdown_link(line.split("##")[1])
        toc.append(link)
    elif line.startswith("#"):
        link = "* " + markdown_link(line.split("#")[1])
        toc.append(link)

# Print TOC and document
for line in toc:
    print(line)
for line in lines: 
    print(line)
