# Date   : 2020-05-16
# Author : Girard Jules | jules.girard@outlook.com

# Load packages
packages_list = c("here", "stringr", "dplyr", "ggplot2")
lapply(packages_list, library, character.only=TRUE)


# Create functions
{
  # Opposite of %in%
  `%!in%` = Negate(`%in%`)
  
  
  # Draw field with ggplot2
  draw_field = function(data = NULL) {
    ggplot(data = data) +
      scale_x_continuous(limits = c(-62, 62), breaks = seq(-60, 60, 15)) +
      scale_y_continuous(limits = c(-40, 40), breaks = seq(-40, 40, 10)) +
      annotate("segment", x = c(-60, -60), xend = c(60, 60), y = c(-40, 40), yend = c(-40, 40)) +
      annotate("segment", x = c(-60, 0, 60), xend = c(-60, 0, 60), y = rep(-40, 3), yend = rep(40, 3)) +
      annotate("segment", x = c(-60, -60, 60, 60), xend = c(-42, -42, 42, 42), y = c(-18, 18, -18, 18), yend = c(-18, 18, -18, 18)) +
      annotate("segment", x = c(-42, 42), xend = c(-42, 42), y = c(-18, -18), yend = c(18, 18)) +
      annotate("segment", x = c(-60, -60, 60, 60), xend = c(-54, -54, 54, 54), y = c(-10, 10, -10, 10), yend = c(-10, 10, -10, 10)) +
      annotate("segment", x = c(-54, 54), xend = c(-54, 54), y = c(-10, -10), yend = c(10, 10)) +
      annotate("segment", x = c(-60, -60, 60, 60), xend = c(-62, -62, 62, 62), y = c(-4, 4, -4, 4), yend = c(-4, 4, -4, 4)) +
      annotate("segment", x = c(-62, 62), xend = c(-62, 62), y = c(-4, -4), yend = c(4, 4)) +
      annotate("point", x = c(-48, 0, 48), y = c(0, 0, 0), size = c(.7, 1, .7)) +
      annotate("path", x = 9.15 * cos(seq(0, 2*pi, length.out = 500)), y = 9.15 * sin(seq(0, 2*pi, length.out = 500))) +
      annotate("path", x = -48 + 9.15 * cos(seq(-8/9.15, 8/9.15, length.out = 100)), y = 9.15 * sin(seq(-8/9.15, 8/9.15, length.out = 100))) +
      annotate("path", x = 48 - 9.15 * cos(seq(-8/9.15, 8/9.15, length.out = 100)), y = 9.15 * sin(seq(-8/9.15, 8/9.15, length.out = 100))) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
  }
  
  
  get_angle = function(x, y) {
    
    ### From x and y event distances travelled on the field,
    ### compute the angle with field X axis between -pi and pi
    ### (both means backward direction, 0 mean forward direction).
    ###
    ### Inputs :
    ###   - x (num), distance travelled on X axis
    ###   - y (num), distance travelled on Y axis
    ###
    ### Output :
    ###   - angle (num), angle in radian
    
    x[is.na(x)] = FALSE
    y[is.na(y)] = FALSE
    
    angle = rep(NA, length(x))
    quadrant = list(x >= 0 & y > 0, x < 0 & y >= 0, x <= 0 & y < 0, x > 0 & y <= 0)
    
    angle[quadrant[2][[1]]] = (pi/2) + atan(-x[quadrant[2][[1]]] / y[quadrant[2][[1]]])  # Q2
    angle[quadrant[1][[1]]] = atan(y[quadrant[1][[1]]] / x[quadrant[1][[1]]])  # Q1
    angle[quadrant[4][[1]]] = -atan(-y[quadrant[4][[1]]] / x[quadrant[4][[1]]])  # Q4
    angle[quadrant[3][[1]]] = -(pi/2) - atan(-x[quadrant[3][[1]]] / -y[quadrant[3][[1]]])  # Q3
    
    return(angle)
  }
  
  
  get_adjusted_angle = function(target_angle, event_angle) {
    
    ### Compute adjusted angle between target (goal) and destination of event.
    ###
    ### Inputs :
    ###   - target.angle (num), angle between goal and player position in radian
    ###   - event.angle (num), angle between destination and player position in radian
    ###
    ### Output :
    ###   - adjusted_angle (num), angle between goal direction and event direction (radian),
    ###        -pi and pi means backward direction, 0 mean forward direction,
    ###        positive values means player left side, negative values means player right side
    
    diff_angle = event_angle - target_angle
    s = sign(diff_angle)
    adjusted_angle = ifelse(s*diff_angle >= pi, -s*(pi - abs(diff_angle - s*pi)), diff_angle)
    
    return(adjusted_angle)
  }
  
  
  logistic_scale = function(value, sigma=10) {
    
    ### Convert values between 0 and 1 to logistic scale.
    ###
    ### Inputs :
    ###   - value (num), values to convert
    ###   - sigma (num, default=10), slope coefficient
    ###
    ### Output :
    ###   - logistic value conversion (num), between 0 and 1
    
    if (sigma == 0) {
      logistic_value = rep(0, length(value))
    } else {
      coef = .5 / abs(.5 - (1 / (1 + exp(-sigma * .5))))
      tmp_logistic_value = (1 / (1 + exp(-sigma * (value - .5)))) - .5
      logistic_value = tmp_logistic_value * coef + .5
      
      logistic_value[value < 0] = ifelse(sign(sigma) > 0, 0, 1)
      logistic_value[value > 1] = ifelse(sign(sigma) > 0, 1, 0)
    }
    
    return(logistic_value)
  }
  
  
  get_CA_target = function(location_X, start_range = c(-42, 20), end_range = c(0, 42)) {
    
    ### Compute the counter attack location_X target
    ###
    ### Inputs :
    ###   - location_X (numeric), position of event on X axis
    ###
    ### Output :
    ###   - target (numeric), position of target on X axis
    
    ratio = ifelse(location_X <= start_range[1], 1, (start_range[2] - location_X) / diff(start_range))
    target = ifelse(ratio >= 0, end_range[2] - (diff(end_range) * ratio), NA)
    
    return(target)
  }
  
  
  get_CA_speed = function(possession, team_off, team_def) {
    
    ### Compute the counter attack speed
    ###
    ### Inputs :
    ###  - possession (data.frame), events of one possession
    ###  - team_off (list), name and index of offensive team
    ###  - team_def (list), name and index of defensive team
    ###  - speed_threshold (numeric vector),
    ###       represent range of speed to compute counter-attack intensity
    ###  - sigma (numeric), between .5 and 1
    ###       parameter to tune the slope of sigmoid function
    ###
    ### Output :
    ###  - intensity (numeric), represent counter-attack intensity of the possession
    
    team_name = as.character(possession$possession_team.name[1])
    team_index = which(possession$team.name == team_name)
    
    x_start = possession$location_X[team_index[1]]
    time_start = possession$time[team_index[1]]
    x_target = get_CA_target(x_start)
    
    cond = possession$location_X[team_index] > x_target
    if (length(which(cond)) == 0) {
      # If the team doesn't reach x_target
      speed = NA
    } else {
      # If the team reach x_target
      x_prev = possession$location_X[head(team_index[which(cond)],1)-1]
      time_prev = possession$time[head(team_index[which(cond)],1)-1]
      x_next = possession$location_X[head(team_index[which(cond)],1)]
      time_next = possession$time[head(team_index[which(cond)],1)]
      
      # Compute speed of movement between the start and the target
      time_target = time_prev + ((time_next - time_prev) * (x_target - x_prev) / (x_next - x_prev))
      speed = (x_target - x_start) / ((time_target - time_start)*60)  # speed m/s
    }
    
    return(speed)
    
    ## Pour définir une contre attaque :
    ## 1. On veut qu'une CA lente se rapproche vers 0, une CA rapide se rapproche vers 1
    ## 2. CA définie par une zone de départ et une zone cible à atteindre : quelle zone cible ?
    ##   2.1. Plus je suis proche de mon camp, plus la distance à parcourir doit être longue (relation linéaire)
    ##        Plus je suis proche de mon camp, plus la zone cible est proche de moi
    ##   2.2. Jusqu'à 20m dans le camp adverse, je peux prétendre à une contre-attaque
    ##        La zone cible reste la même après une récupération dans mes 18m
    ##   2.3. Au plus proche je dois atteindre le milieu du terrain
    ##        Au plus loin je dois atteindre la surface adverse
    ## 3. Je calcule la vitesse de déplacement du ballon entre la récupération et l'atteinte de la zone cible
    ##    Cette vitesse me permettra de définir le degré d'importance de la contre-attaque
  }
}

db_events = read.csv(paste0(here(), "/database/StatsBomb_FIFA_WorldCup2018_Events.csv"))
db_events$possession[which(str_detect(db_events$type.name, 'Camera'))] = NA


db_events = db_events %>%
  # Compute direct play index using :
  #   - pass and carry event
  #   - distance of the event
  #   - angle between event direction and target (goal) direction
  #   - coefficient of event relevance for direct_play : if the event is forward direction
  #     and length longer than target distance, the coefficient decrease according to logistic function
  mutate(
    target.angle = get_angle((60 - location_X), -location_Y),
    target.length = sqrt((60-location_X)^2 + location_Y^2),
    event.length_X = ifelse(type.name == "Pass", pass.end_location_X-location_X, ifelse(type.name == "Carry", carry.end_location_X-location_X, NA)),
    event.length_Y = ifelse(type.name == "Pass", pass.end_location_Y-location_Y, ifelse(type.name == "Carry", carry.end_location_Y-location_Y, NA)),
    event.length = sqrt(event.length_X^2 + event.length_Y^2),
    event.angle = get_angle(x=event.length_X, y=event.length_Y),
    event.adjusted_angle = get_adjusted_angle(target.angle, event.angle),
    coef = ifelse((event.length > target.length) & cos(event.adjusted_angle) > 0 & event.length > 0,
                  logistic_scale(value=(event.length - target.length) / target.length, sigma=-10), 1),
    index.direct_play = event.length * cos(event.adjusted_angle) * coef
  )# %>%
  #select(!c(target.angle, target.length, event.length_X, event.length_Y, event.length, event.angle, event.adjusted_angle, coef))


ggplot(db_events[which(is.na(db_events$index.direct_play) == FALSE),]) +
  geom_point(aes(x=event.angle, y=event.length, color=event.adjusted_angle)) +
  scale_color_gradient2(low="#ab3428", mid="#f5ee9e", high="#2d728f", midpoint=0) +
  scale_x_reverse(name="Pass/carry angle", breaks=c(-pi, -pi/2, 0, pi/2, pi), labels=c("", "Right", "Forward", "Left", "Backward")) +
  scale_y_continuous(name="Pass/carry length") +
  labs(color = "Event adjusted angle") +
  coord_polar(start=-pi)

ggplot(db_events[which(is.na(db_events$index.direct_play) == FALSE),]) +
  geom_point(aes(x=event.angle, y=event.length, color=index.direct_play)) +
  scale_color_gradient2(low="#ab3428", mid="#f5ee9e", high="#2d728f", midpoint=0) +
  scale_x_reverse(name="Pass/carry angle", breaks=c(-pi, -pi/2, 0, pi/2, pi), labels=c("", "Right", "Forward", "Left", "Backward")) +
  scale_y_continuous(name="Pass/carry length") +
  labs(color = "Direct play index") +
  coord_polar(start=-pi)



db_events$CA_speed = NA
for (match.id in unique(db_events$match.id)) {
  print(paste(match.id, length(unique(db_events$match.id)), sep=' / '))
  teams = unique(as.character(db_events$possession_team.name[which(db_events$match.id == match.id)]))
  team_off = list()
  team_def = list()
  for (possession in unique(db_events$possession[which(db_events$match.id == match.id)])) {
    index = which(db_events$match.id == match.id & db_events$possession == possession)
    temp_possession = db_events[index,]
    
    db_events$CA_speed[index] = get_CA_speed(temp_possession)
  }
}

a = db_events %>%
  group_by(match.id, possession) %>%
  summarise(CA_speed = unique(CA_speed)) %>%
  filter(is.na(CA_speed) == FALSE)

ggplot(a, aes(x=CA_speed)) +
  geom_density(aes(y=..density..)) +
  scale_x_continuous(limits=c(-10, 40))




