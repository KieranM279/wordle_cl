setwd('~/Documents/wordle/')

library('ggplot2')
library('ggpubr')
library('stringr')

# Import the dataset for main game
base_data <- read.csv('base_data.csv')
# Import the data for the keyboard
keyboard <- read.csv('keyboard.csv')
# Import all the possible words
words_data <- as.list(read.delim('possible_words.txt'))

# Get list into a manageable format
possible_words <- c()
for (w in words_data) {
  possible_words <-  c(possible_words,w)
}

# Correct the data formats
keyboard$colour_data <- as.factor(keyboard$colour_data)
levels(keyboard$colour_data) <- c("0","1","2","3")
base_data$plot_data <- as.factor(base_data$plot_data)
levels(base_data$plot_data) <- c("0","1","2")

# Create a letter column in the base data set
base_data$letter <- " "

# Define the empty game grid
plot <- ggplot(base_data, aes(x=letter_pos, y=guess, fill=plot_data)) +
  scale_fill_manual(values=c("grey20","gold2","green4")) +
  scale_y_reverse() +
  geom_tile(linejoin="round",size=0.2, color="black", height=0.9, width=0.9) +
  theme(panel.background = element_rect(fill = "black", colour = "black",
                                                size = 0, linetype = "solid"),
        panel.grid.major = element_line(size = 0, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                        colour = "black"),
        plot.background = element_rect(color = "black"),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),legend.position="none",) +
  annotate("rect",xmin=0.5, xmax=5.5, ymin=6.5, ymax=7.5, fill="black")

# Define the empty keybaord
key_plot <- ggplot(keyboard, aes(x=x_pos, y=row,fill=colour_data)) +
  scale_fill_manual(values=c("grey60","gold2","green4", "grey20")) +
  scale_y_reverse() +
  geom_tile(linejoin="round",size=0.2, color="black", height=0.9, width=0.9) +
  geom_text(aes(label=letter), size=12, color="white") +
  theme(panel.background = element_rect(fill = "black", colour = "black",
                                        size = 0, linetype = "solid"),
        panel.grid.major = element_line(size = 0, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                        colour = "black"),
        plot.background = element_rect(color = "black"),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),legend.position="none",) +
  annotate("rect",xmin=2.5, xmax=7.5, ymin=3.5, ymax=4.5, fill="black")

# Plot the empty game grid
output_1 <- ggarrange(key_plot, plot,
                    nrow = 1, ncol = 2)
print(output_1)

######## Required functions ########

#### Generating keyboard colours ####

# Generate the colours data for the keyboard
key_num <- function (input, actual, grow_string, letter) {
  
  # Assume it is unused
  output = "0"
  
  # If the letter has been used by the user
  if (grepl(letter, grow_string, fixed = TRUE)) {
    
    # If the letter is not in the actual word
    if (!grepl(letter, actual, fixed = TRUE)) {
      output = "3"
      
      # If the is in the actual word
    } else if (grepl(letter, actual, fixed = TRUE)) {
      output = "1"
      
      # Loop through the letters of the actual word
      for (i in 1:nchar(actual)) {
        
        # If the position of the letter is correctly guessed
        if (substring(input,i,i) == substring(actual,i,i)) {
          
          # And that letter is the one being addressed
          if (substring(input,i,i) == letter) {
            
            output = "2"
            
          }
        }
      }
    } 
  }
  return(output)
}

#### Generating row colours ####

row_gen <- function (input) {
  
  plot_data <- c()
  
  grow_input <- " "
  
  for (l in 1:nchar(word)) {
    
    
    # Isolate each letter if the guessed word
    letter <- substring(input,l,l)
    
    grow_input <- paste(grow_input,letter)
    
    
    # Find if each letter is present in the word
    if (grepl(letter, word, fixed = TRUE)) {
      
      if (str_count(grow_input, letter) > str_count(word, letter)) {
        
        plot_data <- c(plot_data,"0")
        
      } else {
        
        plot_data <- c(plot_data,"1")
        
      }
    } else {
      
      plot_data <- c(plot_data,"0")
      
    }
    
  }
  
  for (l in 1:nchar(word)) {
    
    # Isolate each letter if the guessed word
    letter <- substring(input,l,l)
    
    # If the letter is also in the correct spot
    if (substring(word,l,l) == letter) {
      plot_data[l] <- "2"
      
    } else {
      next
    }
    
  }
  
  
  
  return(plot_data)
}

letter_gen <- function(input) {
  
  row_letter <- c()
  
  for (i in 1:nchar(input)) {
    letter <- substring(input,i,i)
    
    row_letter <- c(row_letter,letter)
    
  }
  
  return(row_letter)
  
}

word <- toupper(sample(possible_words,1))



grow_string <- " "

i = 0

while (i < 6) {

  
  
  guess_a <- readline(prompt="Enter guess: ")
  
  if (guess_a %in% possible_words) {
    i = (i + 1)
  } else {
    print("That is not an allowed word, idiot!!!!!")
    next
  }
  
  guess_a <- toupper(guess_a)
  
  grow_string <- paste(grow_string, guess_a, sep = "")
  
  
  base_data$plot_data[(((i-1)*5)+1):(i*5)] <- as.factor(row_gen(guess_a))
  levels(base_data$plot_data) <- c("0","1","2")
  base_data$letter[(((i-1)*5)+1):(i*5)] <- letter_gen(guess_a)
  
  
  for (k in 1:nrow(keyboard)) {
    
    if (keyboard$letter[k] == ".") {
      next
    } else if (keyboard$colour_data[k] == 2) {
      next
    }  else {
      keyboard$colour_data[k] <- key_num(guess_a, word, grow_string, keyboard$letter[k])
    }
  }
  
  
  
  
  
  key_plot <- ggplot(keyboard, aes(x=x_pos, y=row,fill=colour_data)) +
    scale_fill_manual(values=c("grey60","gold2","green4", "grey20")) +
    scale_y_reverse() +
    geom_tile(linejoin="round",size=0.2, color="black", height=0.9, width=0.9) +
    geom_text(aes(label=letter), size=12, color="white") +
    theme(panel.background = element_rect(fill = "black", colour = "black",
                                          size = 0, linetype = "solid"),
          panel.grid.major = element_line(size = 0, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                          colour = "black"),
          plot.background = element_rect(color = "black"),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.title.y=element_blank(),legend.position="none",) +
    annotate("rect",xmin=2.5, xmax=6.5, ymin=3.5, ymax=4.5, fill="black")
  
  plot <- ggplot(base_data, aes(x=letter_pos, y=guess, fill=plot_data)) +
    scale_fill_manual(values=c("grey20","gold2","green4")) +
    geom_tile(linejoin="round",size=0.2, color="black", height=0.9, width=0.9) +
    geom_text(aes(label=letter), size=15, color="white") +
    scale_y_reverse() +
    theme(panel.background = element_rect(fill = "black", colour = "black",
                                          size = 0, linetype = "solid"),
          panel.grid.major = element_line(size = 0, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                          colour = "black"),
          plot.background = element_rect(color = "black"),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.title.y=element_blank(),legend.position="none",) +
    annotate("rect",xmin=0.5, xmax=5.5, ymin=6.5, ymax=7.5, fill="black")
  
  output <- ggarrange(key_plot, plot,
                      nrow = 1, ncol = 2)
  print(output)
  
  print(row_gen(guess_a))
  
  if (guess_a == word) {
    print("Yayyyy! you got the correct answer!")
    break
  }

}

