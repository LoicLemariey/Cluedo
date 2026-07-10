#config

n_dimension<-3



characters <- c(
    "Miss Scarlett",
    "Colonel Mustard",
    "Mrs. White",
    "Mr. Green",
    "Mrs. Peacock",
    "Professor Plum"
)

armes <- c(
    "Candlestick",
    "Dagger",
    "Lead Pipe",
    "Revolver",
    "Rope",
    "Wrench"
)

lieux <- c(
    "Kitchen",
    "Ballroom",
    "Conservatory",
    "Dining Room",
    "Billiard Room",
    "Library",
    "Lounge",
    "Hall",
    "Study"
)

response_choices <- c(
    "Not asked",
    "No card",
    "One of the three",
    "Character",
    "Weapon",
    "Room"
)

cartes<-c(lieux,armes,characters)

cartes_df<-data.frame(cartes=cartes,
                      type=c(rep("lieux",length(lieux)),
                             rep("armes", length(armes)),
                             rep("suspect", length(characters))))

n_joueurs<-4