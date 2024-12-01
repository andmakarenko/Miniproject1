random = Math.random
floor = Math.floor

# Function to generate the trick list
generateTrickList = (n, patternLen) ->
  nums = []
  pattern = []

  # Generate a pattern of random numbers of length patternLen
  for i in [0...patternLen]
    pattern.push floor(random() * 10)

  # Generate the full list of random numbers, sometimes inserting the pattern
  i = 0
  while i < n
    if i + patternLen < n and random() < 0.01
      for j in [0...patternLen]
        nums.push pattern[j]
      i += patternLen
    else
      nums.push floor(random() * 10)
      i += 1

  return nums

# Function to find time loops within the list
findTimeLoops = (nums, len) ->
  patternFrequency = {}

  for i in [0..(nums.length - len)]
    pattern = (nums[j] for j in [i...(i + len)]).join(",")

    # Update the frequency of the pattern
    patternFrequency[pattern] ?= 0
    patternFrequency[pattern] += 1

  # Filter out patterns that occur only once
  for key, value of patternFrequency
    delete patternFrequency[key] if value < 2

  return patternFrequency

# Function to look for outliers within the found patterns
lookForOutliers = (timeLoops) ->
  avg = 0.0
  max = 0
  maxKey = ""

  console.log "\nGefundene Zeitschleifen in timeLoops:"
  if Object.keys(timeLoops).length is 0
    console.log "Keine Zeitschleifen gefunden."
  else
    for key, value of timeLoops
      console.log "Muster: #{key} | Wiederholungen: #{value}"

  # Calculate the average and find the maximum frequency
  for key, value of timeLoops
    avg += value
    if value > max
      max = value
      maxKey = key

  avg = avg / Object.keys(timeLoops).length

  # Determine if an outlier exists based on the average
  if max - avg > 3
    console.log "\nVermeintliche Zeitschleife gefunden!"
    console.log "#{maxKey} wurde #{max} mal wiederholt."
  else
    console.log "\nKeine vermeintliche Zeitschleife gefunden!"

# Main execution
main = ->
  trickNums = generateTrickList 1000, 5
  len = 5
  trickTimeLoops = findTimeLoops trickNums, len
  lookForOutliers trickTimeLoops

# Run the main function
main()
