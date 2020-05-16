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
  
  
  get_directPlay_coef = function(event_length, target_length, event_adjusted_angle, sigma=5) {
    
    ### Compute the relevance of the event comparing event length to target distance.
    ### If event length is greater than target distance the coefficient decrease.
    ### Coefficient is a measure of the ratio between extra distance covered by the event
    ### and the target distance, then using logistic function to get coefficient :
    ###     - if extra-distance is less than target distance (or backward direction), coeffcient is 1
    ###     - if extra-distance is half target distance, coefficient is 0.5
    ###     - if extra-distance is same as target distance, coefficient is 0
    ### 
    ### Inputs :
    ###  - event_length (num), length of the event (pass / carry)
    ###  - target_length (num), distance between player and target
    ###  - event_adjusted_angle (num), adjusted angle between target direction and event direction
    ###  - sigma (num, default=5), greater than 0, manage logistic slope (the lower the smoother)
    ###
    ### Output :
    ###  - coefficient (num), between 0 (too long event) and 1 (less or equal to target distance)
    
    if (sigma < 0) stop(paste0("'sigma' should be positive : sigma=", sigma))
    
    coef = ifelse((event_length > target_length) & cos(event_adjusted_angle) > 0,
                  (1 / (1 + exp(sigma * ((((event_length - target_length) / target_length) * 2) - 1)))),
                  1)
    
    return(coef)
  }
}

db_events = read.csv(paste0(here(), "/database/StatsBomb_FIFA_WorldCup2018_Events.csv"))
db_events$possession[which(str_detect(db_events$type.name, 'Camera'))] = NA


db_events = db_events %>%
  # Compute direct play index using :
  #   - pass and carry event
  #   - distance of the event
  #   - angle between event direction and target (goal) direction 
  mutate(
    target.angle = get_angle((60 - location_X), -location_Y),
    target.length = sqrt((60-location_X)^2 + location_Y^2),
    event.length_X = ifelse(type.name == "Pass", pass.end_location_X-location_X, ifelse(type.name == "Carry", carry.end_location_X-location_X, NA)),
    event.length_Y = ifelse(type.name == "Pass", pass.end_location_Y-location_Y, ifelse(type.name == "Carry", carry.end_location_Y-location_Y, NA)),
    event.length = sqrt(event.length_X^2 + event.length_Y^2),
    event.angle = get_angle(x=event.length_X, y=event.length_Y),
    event.adjusted_angle = get_adjusted_angle(target.angle, event.angle),
    coef = get_directPlay_coef(event.length, target.length, event.adjusted_angle),
    index.direct_play = event.length * cos(event.adjusted_angle) * coef
  ) %>%
  select(!c(target.angle, target.length, event.length_X, event.length_Y, event.length, event.angle, event.adjusted_angle, coef))


# ggplot(db_events[which(db_events$type.name == "Pass" | db_events$type.name == "Carry"),]) +
#   geom_segment(aes(x=0, y=0, xend=event.length_X, yend=event.length_Y, color=event.adjusted_angle), size=.05) +
#   scale_color_gradient(low='purple', high='yellow') +
#   scale_x_continuous(name="Left/right component") +
#   scale_y_continuous(name="Forward/backward component") +
#   labs(color = "Pass/carry adjusted angle")
# 
# ggplot(db_events[which(db_events$type.name == "Pass" | db_events$type.name == "Carry"),]) +
#   geom_point(aes(x = event.adjusted_angle, y = event.length, color=index.direct_play)) +
#   scale_color_gradient(low='purple', high='yellow') +
#   scale_x_continuous(name="Pass/carry adjusted angle") +
#   scale_y_continuous(name="Pass/carry length") +
#   labs(color = "Direct play index")

