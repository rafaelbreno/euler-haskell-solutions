#!/bin/bash

# Pad (padding_char, length, value)
# Got this from ChatGPT
pad() {
    local padding_char="$1"
    local desired_length="$2"
    local value="$3"
    
    local padding_length=$(( desired_length - ${#value} ))
    local padding=""
    
    for ((i=0; i<padding_length; i++)); do
        padding="${padding}${padding_char}"
    done
    
    echo "${padding}${value}"
}

problem=$1
padded_problem=$(pad "0" 3 $problem)

min_url="https://projecteuler.net/minimal=$problem"
prob_url="https://projecteuler.net/minimal=$problem"

response=$(curl -s $min_url)

mainFile="$(cat "./000/Main.hs")"
readmeFile="## Problem $padded_problem

$response

[URL]($prob_url)
[Solution](./Main.hs)"

folder_path="$padded_problem"

if [ -d "$folder_path" ]; then
    echo "Problem $padded_problem already setup."
    return
fi

mkdir $folder_path

echo "$mainFile" >> "$folder_path/Main.hs"
echo "$readmeFile" >> "$folder_path/README.md"
