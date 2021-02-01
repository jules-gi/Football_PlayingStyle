# Date   : 2020-06-12
# Author : Girard Jules | jules.girard@outlook.com

# Load packages
packages_list = c("here", "stringr", "dplyr", "ggplot2")
lapply(packages_list, library, character.only=TRUE)


# Create functions
{
  ### GENERAL FUNCTIONS ###
  
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
  
  
  ### Validation function
  
  check_area_format = function(area) {
    
    # To close the area, its first and last point should be the same 
    if (area[1, 1] != area[nrow(area), 1] | area[1, 2] != area[nrow(area), 2]) area = rbind(area, area[1,])
    
    return(area)
  }
  
  
  check_point_2Dformat = function(point, list) {
    
    # Check if point format is a list of numeric vectors of size 2,
    # or a unique numeric vector of size 2.
    size_msg = "Coordinates of point must be size 2."
    # Check if point vectors is not NA
    na_msg = "NA values detected in points vectors."
    
    if (is.list(point)) {
      if (max(sapply(point, length)) > 2) stop(size_msg)
      x = sapply(point, `[`, 1)
      y = sapply(point, `[`, 2)
    } else {
      if (length(point) > 2) stop(size_msg)
      x = point[1]
      y = point[2]
    }
    if (any(is.na(x)) | any(is.na(y))) stop(na_msg)
    
    return(list(x = x, y = y))
  }
  
  
  ### GEOMETRY FUNCTIONS ###
  
  is_collinear = function(point, start, end) {
    
    ### Check if a 2D point is collinear with a given segment.
    ###
    ### Inputs :
    ###   - point (num vector, or list of num vector)
    ###       A given vector represent (x, y) point coordinates
    ###   - start (num), (x, y) coordinates segment start point
    ###   - end (num), (x, y) coordinates of segment end point
    ###
    ### Outputs :
    ###   - boolean, TRUE if the points are collinear else FALSE
    
    point = check_point_2Dformat(point)
    
    if (is.list(point)) {
      x = sapply(point, `[`, 1)
      y = sapply(point, `[`, 2)
    } else {
      x = point[1]
      y = point[2]
    }
    
    area = start[1] * (y - end[2]) + x * (end[2] - start[2]) + end[1] * (start[2] - y)
    
    return(area == 0)
  }
  
  
  on_segment = function(point, start, end) {
    
    ### Check if a 2D point is on a given segment.
    ###
    ### Inputs :
    ###   - point (num vector, or list of num vector)
    ###       A given vector represent (x, y) point coordinates
    ###   - start (num), (x, y) coordinates segment start point
    ###   - end (num), (x, y) coordinates of segment end point
    ###
    ### Outputs :
    ###   - boolean, TRUE if point q is on segment [pr] else FALSE
    
    check_point_2Dformat(point)
    
    if (is.list(point)) {
      x = sapply(point, `[`, 1)
      y = sapply(point, `[`, 2)
    } else {
      x = point[1]
      y = point[2]
    }
    
    collinear_point = is_collinear(point=point, start=start, end=end)
    on_condition = (x <= max(start[1], end[1]) & x >= min(start[1], end[1]) & y <= max(start[2], end[2]) & y >= min(start[2], end[2]))
    
    return(collinear_point & on_condition)
  }
  
  # start = c(-1, -1) ; point = list(c(1, 0), c(0, 0), c(0, 1), c(2, 2)) ; end = c(1, 1)
  # get_angle_orientation(point=point, start=start, end=end)
  get_angle_orientation = function(p, q, r) {
    
    ### Compute the orientation of (pqr) angle
    ###
    ### Inputs :
    ###   - p (num), (x, y) coordinates of start point
    ###   - q (num), (x, y) coordinates of middle point
    ###   - r (num), (x, y) coordinates of end point
    ###
    ### Outputs :
    ###   - orientation index :
    ###       - 0 : points are collinear
    ###       - 1 : clockwise orientation
    ###       - -1 : anticlockwise orientation
    
    check_point_2Dformat(point)
    
    val = (start[2] - p[2]) * (end[1] - start[1]) - (start[1] - p[1]) * (end[2] - start[2])
    orientation = if (val == 0) 0 else if (val > 0) 1 else -1
    
    return(orientation)
  }
  
  
  is_intersect = function(a1, z1, a2, z2, collinear.rm=FALSE) {
    
    ### Check if [a1, z1] intersect with [a2, z2]
    ###
    ### Inputs :
    ###   - a1 (num), (x, y) coordinates
    ###   - z1 (num), (x, y) coordinates
    ###   - a2 (num), (x, y) coordinates
    ###   - z2 (num), (x, y) coordinates
    ###   - collinear.rm (bool), if intersect and collinear segment should return TRUE
    ###
    ### Output :
    ###   - boolean, TRUE if segments intersect else FALSE
    
    dir_a2 = get_angle_orientation(a1, z1, a2)
    dir_z2 = get_angle_orientation(a1, z1, z2)
    dir_a1 = get_angle_orientation(a2, z2, a1)
    dir_z1 = get_angle_orientation(a2, z2, z1)
    
    intersect = FALSE
    if ((dir_a2 != dir_z2) & (dir_a1 != dir_z1)) {
      # Global case
      intersect = TRUE
    } else if (dir_a2 == 0 & on_segment(a1, a2, z1) & collinear.rm == FALSE) {
      # a1, a2, z1 are collinear and a2 is on segment [a1, z1]
      intersect = TRUE
    } else if (dir_z2 == 0 & on_segment(a1, z2, z1) & collinear.rm == FALSE) {
      # a1, z2, z1 are collinear and z2 is on segment [a1, z1]
      intersect = TRUE
    } else if (dir_a1 == 0 & on_segment(a2, a1, z2) & collinear.rm == FALSE) {
      # a2, a1, z2 are collinear and a1 is on segment [a2, z2]
      intersect = TRUE
    } else if (dir_z1 == 0 & on_segment(a2, z1, z2) & collinear.rm == FALSE) {
      # a2, z1, z2 are collinear and z1 is on segment [a2, z2]
      intersect = TRUE
    }
    
    return(intersect)
  }
  
  
  is_in_area = function(x, y, area, point_projection=10000) {
    
    ### Check if points lies inside a given area
    ###
    ### Inputs :
    ###   - x (num vector), x coordinate of points
    ###   - y (num vector), y coordinate of points
    ###   - area (data.frame), corners points of a given 2D polygon
    ###       Each row represent a point, columns represent their (x, y) coordinates
    ###
    ### Output :
    ###   - in_area (bool vector), same lengtn as x and y, TRUE if the point lies
    ###       inside the area else FALSE
    
    # Check if x and y coordinates have the same length
    if (length(x) != length(y)) stop(paste0("Unequal area parameters lengths: x (", length(x), "), y (", length(y), ")"))
    
    area = check_area_format(area)
    
    # Get number of borders area
    n_borders = nrow(area) - 1
    if (n_borders < 3) stop("There must be at least 3 vertices to create the area.")
    
    in_area = NULL
    for (i in 1:length(x)) {
      print(paste(i, "/", length(x)))
      event = c(-45, -10) #c(x[i], y[i])
      event.right_proj = c(point_projection, event[2])# c(point_projection, y[i])
      event.left_proj = c(-point_projection, event[2])# c(-point_projection, y[i])
      
      # Count number of border crossed
      cross_count.right_proj = 0
      cross_count.left_proj = 0
      for (j in 1:n_borders) {
        
        if (on_segment(p=as.numeric(area[j,]), q=event, r=as.numeric(area[j+1,]))) {
          cross_count.right_proj = 1
          cross_count.left_proj = 1
          break
        }
        
        if (is_intersect(a1=event, z1=event.right_proj, a2=as.numeric(area[j,]), z2=as.numeric(area[j+1,]), collinear.rm=FALSE)) {
          # print(c("Right: ", paste(as.numeric(area[j,]), as.numeric(area[j+1,]), sep=", ")))
          cross_count.right_proj = cross_count.right_proj + 1
        }
        if (is_intersect(a1=event, z1=event.left_proj, a2=as.numeric(area[j,]), z2=as.numeric(area[j+1,]), collinear.rm=FALSE)) {
          # print(c("Left: ", paste(as.numeric(area[j,]), as.numeric(area[j+1,]), sep=", ")))
          cross_count.left_proj = cross_count.left_proj + 1
        }
      }
      # print((cross_count.right_proj%%2 == 1 & cross_count.left_proj%%2 == 1))
      
      
      in_area = c(in_area, (cross_count.right_proj%%2 == 1 & cross_count.left_proj%%2 == 1))
    }
    
    return(in_area)
  }
  x = rep(-60:60, 81)
  y = rep(-40:40, each=121)
  area = data.frame(
    x = c(-60, -60, -42, -42, -60, -60, -15, 0, 15, 60, 60, 42, 42, 60, 60, 15, 0, -15, -60),
    y = c(-40, -18, -18, 0, 0, 40, 40, 10, 40, 40, 18, 18, 0, 0, -40, -40, -10, -40,-40)
  )
  
  # in_area = is_in_area(x=x, y=y, area=area)
  # draw_field() +
  #   geom_polygon(data = area, aes(x=x, y=y), fill="blue", alpha=.3)
  #   #annotate("point", x=x, y=y, color=ifelse(in_area, "green", "red"), size=.5)
  
  
  get_border_crossed = function(a1, z1, area) {
    
    ### Get the position where a segment comes in or out of the area
    ###
    ### Inputs :
    ###   - a1 (num), (x, y) coordinates
    ###   - z1 (num), (x, y) coordinates
    ###   - area (data.frame), corners points of a given 2D polygon
    ###       Each row represent a point, columns represent their (x, y) coordinates
    ###
    ### Output :
    ###   - common point (num), (x, y) coordinates of the point where segment cross
    
    area = check_area_format(area)
    
    n_borders = nrow(area) - 1
    cross_point = data.frame(x=NULL, y=NULL)
    for (i in 1:n_borders) {
      a2 = as.numeric(area[i,])
      z2 = as.numeric(area[i+1,])
      
      # Check if segment intersect
      if (is_intersect(a1, z1, a2, z2)) {
        u = ((z2[1] - a2[1]) * (a1[2] - a2[2]) - (z2[2] - a2[2]) * (a1[1] - a2[1])) / 
          ((z2[2] - a2[2]) * (z1[1] - a1[1]) - (z2[1] - a2[1]) * (z1[2] - a1[2]))
        
        cross_point = rbind(cross_point, data.frame(x=a1[1] + u*(z1[1] - a1[1]), y=a1[2] + u*(z1[2] - a1[2])))
      }
    }
    
    return(cross_point)
  }
  
  
  ### PLAYING STYLE FUNCTIONS ###
  
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
  
  
  time_in_area = function(x, y, time, area, return_index=FALSE) {
    
    ### Compute the time spent in a given area
    ###
    ### Inputs :
    ###   - x (num vector), x coordinate of event position
    ###   - y (num vector), y coordinate of event position
    ###   - time (num vector), time when event occured
    ###   - area (data.frame), corners points of a given 2D polygon
    ###       Each row represent a point, columns represent their (x, y) coordinates
    ###
    ### Output :
    ###   - time (num), time spent in the area
    
    # Get first and last index of each continuous situation where location are in area
    in_area = is_in_area(x=x, y=y, area=area)
    
    if (any(in_area)) {
      first = which(diff(in_area) == 1) + 1
      if (in_area[1]) first = c(1, first)
      last = which(diff(in_area) == -1)
      if (in_area[length(in_area)]) last = c(last, length(x))
      
      time_in_area = NULL
      for (i in 1:length(first)) {
        
        # Get event location
        entry.next_location = c(x[first[i]], y[first[i]])
        entry.prev_location = if (first[i] == 1) NULL else c(x[first[i] - 1], y[first[i] - 1])
        exit.prev_location = c(x[last[i]], y[last[i]])
        exit.next_location = if (last[i] == length(x)) NULL else c(x[last[i] + 1], y[last[i] + 1])
        
        if (is.null(entry.prev_location)) {
          # If possession start in the area
          entry.time = time[first[i]]
        } else {
          # Get location where area border is crossed first
          entry.location = get_border_crossed(a1=entry.next_location, z1=entry.prev_location, area)
          entry.dist = sqrt(rowSums(sweep(entry.location, 2, entry.next_location)^2))
          entry.index = which.max(entry.dist)
          
          # Get entry time
          entry.location = as.numeric(entry.location[entry.index,])
          entry.coef = entry.dist[entry.index] / sqrt(sum((entry.prev_location - entry.next_location)^2))
          entry.time = time[first[i]] - entry.coef * (time[first[i]] - time[first[i] - 1])
        }
        
        if (is.null(exit.next_location)) {
          # If the possession finish in the area
          exit.time = time[last[i]]
        } else {
          # Get location where area border is crossed last
          exit.location = get_border_crossed(a1=exit.prev_location, z1=exit.next_location, area)
          exit.dist = sqrt(rowSums(sweep(exit.location, 2, exit.prev_location)^2))
          exit.index = which.max(exit.dist)
          
          # Get exit time
          exit.location = as.numeric(exit.location[exit.index,])
          exit.coef = exit.dist / sqrt(sum((exit.prev_location - exit.next_location)^2))
          exit.time = time[last[i]] + exit.coef * (time[last[i] + 1] - time[last[i]])
        }
        
        time_in_area = c(time_in_area, exit.time - entry.time)
      }
      
      if (return_index) return(list(time=time_in_area, index = list(first=first, last=last))) else return(time_in_area)
    }
    
    return(NA)
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
  
  
  get_CA_speed = function(x, time) {
    
    ### Compute the counter attack speed
    ###
    ### Inputs :
    ###  - x (num vector), offensive event X location
    ###  - time (num vector), offensive event time
    ###
    ### Output :
    ###  - speed (numeric), speed attacking progression of offensive team
    
    start_x = x[1]
    start_time = time[1]
    target_x = get_CA_target(start_x)
    
    cond = x > target_x
    if (length(which(cond)) == 0) {
      # If the team doesn't reach x_target
      speed = NA
    } else {
      # If the team reach x_target
      prev_x = x[head(which(cond),1)-1]
      prev_time = time[head(which(cond),1)-1]
      next_x = x[head(which(cond),1)]
      next_time = time[head(which(cond),1)]
      
      # Compute speed of movement between the start and the target
      target_time = prev_time + ((next_time - prev_time) * (target_x - prev_x) / (next_x - prev_x))
      speed = (target_x - start_x) / ((target_time - start_time)*60)  # speed m/s
    }
    
    return(speed)
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
  ) %>%
  select(!c(target.angle, target.length, event.length_X, event.length_Y, event.length, event.angle, event.adjusted_angle, coef))


# Set area location

area_BU = data.frame(
  x = c(0, 0, 60, 60, 42, 42, 60, 60),
  y = c(-40, 40, 40, 18, 18, -18, -18, -40)
)

area_MTN = data.frame(
  x = c(-60, -60, 9.15, 9.15),
  y = c(-40, 40, 40, -40)
)

draw_field() +
  geom_polygon(data = area, aes(x=x, y=y), fill="blue", alpha=.3)

# Run playing style processing per possession
{
  db_events$CA.start = NA
  db_events$CA.speed = NA
  db_events$CA.length = NA
  db_events$MTN.time = NA
  db_events$BU.time = NA
  for (match.id in unique(db_events$match.id)) {
    print(paste(match.id, length(unique(db_events$match.id)), sep=' / '))
    
    teams = unique(as.character(db_events$possession_team.name[which(db_events$match.id == match.id)]))
    team_off = list()
    team_def = list()
    possession_list = unique(db_events$possession[which(db_events$match.id == match.id)])
    for (possession in possession_list[which(!is.na(possession_list))]) {
      index = which(db_events$match.id == match.id & db_events$possession == possession)
      temp_possession = db_events[index,]
      
      team_name = as.character(temp_possession$possession_team.name[1])
      team_index = which(temp_possession$team.name == team_name & is.na(temp_possession$location_X) == FALSE)
      
      start_x = temp_possession$location_X[team_index[1]]
      db_events$CA.start[index] = start_x
      db_events$CA.length[index] = get_CA_target(start_x) - start_x
      db_events$CA.speed[index] = get_CA_speed(x=temp_possession$location_X[team_index],
                                               time=temp_possession$time[team_index])
      
      db_events$MTN.time[index] = max(time_in_area(x=temp_possession$location_X[team_index],
                                                   y=temp_possession$location_Y[team_index],
                                                   time=temp_possession$time[team_index],
                                                   area=area_MTN))
      db_events$BU.time[index] = max(time_in_area(x=temp_possession$location_X[team_index],
                                                  y=temp_possession$location_Y[team_index],
                                                  time=temp_possession$time[team_index],
                                                  area=area_BU))
      
    }
  }
}


### Tests

x = temp_possession$location_X[team_index]
y = temp_possession$location_Y[team_index]
time = temp_possession$time[team_index]

a = db_events %>%
  group_by(match.id, possession) %>%
  summarise(CA_speed = unique(CA_speed)) %>%
  filter(is.na(CA_speed) == FALSE)

ggplot(a, aes(x=CA_speed)) +
  geom_density(aes(y=..density..)) +
  scale_x_continuous(limits=c(-10, 40))




### Examples

{ # Direct play
  pass = db_events %>%
    select(type.name, location_X, location_Y, pass.end_location_X, pass.end_location_Y, carry.end_location_X, carry.end_location_Y) %>%
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
    ) %>%
    filter(!is.na(index.direct_play))
  
  
  ggplot(pass) +
    geom_point(aes(x=event.angle, y=event.length, color=event.adjusted_angle)) +
    scale_color_gradientn(colours=c("#8a2012", "#FFFFFF", "#208a2c", "#FFFFFF", "#8a2012"), breaks=c(-pi, 0, pi), labels=c("-pi", "0", "pi")) +
    scale_x_reverse(name="Pass/carry angle", breaks=c(-pi, -pi/2, 0, pi/2, pi), labels=c("", "Right", "Forward", "Left", "Backward")) +
    scale_y_continuous(name="Pass/carry distance") +
    labs(color = "Event adjusted angle") +
    coord_polar(start=-pi) +
    ggtitle("Adjusted angle given the original pass/carry angle") +
    theme_linedraw() + 
    theme(
      panel.border = element_rect(color=NA)
    )
  ggsave('plot/adjusted_angle.png')
  
  ggplot(pass) +
    geom_point(aes(x=event.angle, y=event.length, color=index.direct_play)) +
    scale_color_gradient2(low="#8a2012", mid="#FFFFFF", high="#208a2c", midpoint=0) +
    scale_x_reverse(name="Pass/carry angle", breaks=c(-pi, -pi/2, 0, pi/2, pi), labels=c("", "Right", "Forward", "Left", "Backward")) +
    scale_y_continuous(name="Pass/carry distance") +
    labs(color = "Direct play index") +
    coord_polar(start=-pi) +
    ggtitle("Direct play index given the pass angle and distance on the field") +
    theme_linedraw() +
    theme(
      panel.border = element_rect(color=NA)
    )
  ggsave('plot/direct_play_index.png')
}

{ # Find event occured in a given area
  area = data.frame(
    x = c(0, 0, 60, 60, 42, 42, 60, 60),
    y = c(-40, 40, 40, 18, 18, -18, -18, -40)
  )
  
  match.id = 1 ; possession = 37
  index = which(db_events$match.id == match.id & db_events$possession == possession)
  temp_possession = db_events[index,]
  team_name = as.character(temp_possession$possession_team.name[1])
  team_index = which(temp_possession$team.name == team_name)
  
  x = temp_possession$location_X[team_index]
  y = temp_possession$location_Y[team_index]
  time = temp_possession$time[team_index]
  time_in_area(x, y, time, area=area_MTN)
  
  draw_field() +
    geom_polygon(data = area, aes(x=x, y=y), fill="blue", alpha=.3) +
    annotate("segment", x = x[-length(x)], xend = x[-1], y = y[-length(y)], yend = y[-1]) + 
    annotate("point", x = x, y = y, color = ifelse(is_in_area(x=x, y=y, area=area), "green", "red"))
}

