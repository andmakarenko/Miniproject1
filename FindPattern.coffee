# Generate an array of 1000 random integers between 1 and 9
numbers = (Math.floor(Math.random() * 9) + 1 for i in [0...1000])

# Calculate the frequency of each number
frequency = {}
for num in numbers
  frequency[num] = (frequency[num] or 0) + 1

# Print the frequency of each number
console.log "Frequency of each number:"
for num, count of frequency
  console.log "#{num}: #{count} times"

# Find repeated sequences of length 3
sequenceMap = {}
for i in [0...numbers.length - 2]
  sequence = "#{numbers[i]},#{numbers[i + 1]},#{numbers[i + 2]}"
  sequenceMap[sequence] = (sequenceMap[sequence] or 0) + 1

# Print sequences repeated more than 4 times
console.log "\nRepeated sequences of length 3 that come up more than 4 times:"
for sequence, count of sequenceMap
  if count > 4
    console.log "#{sequence} appears #{count} times"
