# Date   : 2020-03-03
# Author : Girard Jules (jules.girard@outlook.com)

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
  
  
  get_CA_target = function(location_X, start_range = c(-42, 20), end_range = c(9.15, 42)) {
    
    ### Compute the counter attack location_X target
    ###
    ### Inputs :
    ###   - location_X (numeric), position of event on X axis
    ### Output :
    ###   - target (numeric), position of target on X axis
    
    ratio = ifelse(location_X <= start_range[1], 1, (start_range[2] - location_X) / diff(start_range))
    target = ifelse(ratio >= 0, end_range[2] - (diff(end_range) * ratio), NA)
    
    return(target)
  }
}

db_events = read.csv(paste0(here(), "/database/StatsBomb_FIFA_WorldCup2018_Events.csv"))
db_events$possession[which(str_detect(db_events$type.name, 'Camera'))] = NA

# Add pass.direct_play
db_events = db_events %>%
  mutate(pass.direct_play = abs(pass.end_location_X-location_X) * cos(pass.angle))


# ggplot(db_events[which(db_events$type.name == "Pass"),],
#        aes(x = pass.angle, y = pass.direct_play, color = pass.length),
#        alpha = .2, size = .03) +
#   geom_point() +
#   scale_color_gradient(low = 'orange', high = 'blue')
# 
# ggplot(db_events[which(db_events$type.name == "Pass"),],
#        aes(x = pass.direct_play, y = pass.end_location_X-location_X,
#            color = (pass.end_location_X-location_X)-pass.direct_play),
#        alpha = .2, size = .03) +
#   geom_point() +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'orange')
# 
# ggplot(db_events[which(db_events$type.name == "Pass" & db_events$pass.direct_play > 0),],
#        aes(x = pass.end_location_X-location_X)) +
#   geom_density(aes(y = max(..density..)-..density..))


x_start = 20
get_CA_target(x_start)

draw_field(db_events[which(db_events$type.name == "Shot"),]) +
  annotate("segment", x = get_CA_target(x_start), xend = get_CA_target(x_start), y = -40, yend = 40, color = "dark blue", size = 2) +
  annotate("segment", x = x_start, xend = x_start, y = -40, yend = 40, color = "red", linetype = "dashed", size = 1)

start_range = seq(-42, 20, 1)
end_range = seq(10, 42, 1)
target = get_CA_target(start_range)
dist = target - start_range
plot(start_range, dist)


db_events = db_events %>%
  group_by(match.id, possession) %>%
  mutate(x.start = head(location_X, 1),
         x.target = get_CA_target(x.start))

a$prev.target = NA
a$next.target = NA
for (match.id in unique(a$match.id)) {
  teams = unique(as.character(a$possession_team.name[which(a$match.id == match.id)]))
  team_off = list()
  team_def = list()
  for (possession in unique(a$possession[which(a$match.id == match.id)])) {
    index = which(a$match.id == match.id & a$possession == possession)
    team_off$name = as.character(a$possession_team.name[index[1]])
    team_off$index = index[which(a$team.name[index] == team_off$name)]
    team_def$name = teams[teams != team_off$name]
    team_def$index = index[which(a$team.name[index] == team_def$name)]
    
    cond = a$location_X[index] > a$x.target[index]
    a$prev.target[index] = ifelse(length(which(cond)) > 0, a$location_X[index[which(cond)[1]]-1], NA)
    a$next.target[index] = ifelse(length(which(cond)) > 0, a$location_X[index[which(cond)[1]]], NA)
  }
}


a %>%
  group_by(match.id, possession) %>%
  mutate(next.target = head(location_X[location_X > x.target], 1))
         #, next.target = location_X > x.target)

# features_ca = db_events %>%
#   select(match.id, possession, possession_team.name, time, team.name, type.name, location_X) %>%
#   filter(team.name == head(possession_team.name, 1), type.name != "Carry") %>%
#   group_by(match.id, possession) %>%
#   mutate(distance = location_X - head(location_X, 1), diff_time = time*60 - head(time*60, 1), speed = distance/diff_time) %>%
#   filter(diff_time != 0, distance == max(distance))
# 
# as.data.frame(table(features_ca$speed))
# 
# ggplot(features_ca, aes(x = distance, y = speed, color = diff_time)) +
#   geom_point() +
#   #scale_x_continuous(limits = c(0, 120)) +
#   #scale_y_continuous(limits = c(0, 30)) +
#   theme_minimal()
# 
# ggplot(features_ca, aes(x = speed)) +
#   stat_ecdf(geom = "step") +
#   scale_x_continuous(limits = c(0, 20)) +
#   scale_y_continuous() +
#   theme_minimal()


temp = db_events[which(db_events$match.id == 0 & db_events$possession == 123),]
plot = temp %>%
  mutate(dist.start = location_X - location_X[1],
         dist.prev = c(0, diff(location_X)),
         time.start = (time - time[1]) * 60,
         time.prev = c(0, diff(time)) * 60,
         speed.start = dist.start / time.start,
         speed.prev = dist.prev / time.prev) %>%
  filter(team.name == possession_team.name[1]) %>%
  select(dist.start, dist.prev, time.start, time.prev, speed.start, speed.prev)

ggplot(plot, aes(x = dist.start, y = speed.start)) +
  #geom_bar(stat = "identity") +
  geom_line()
  #geom_point(aes(color = time.start))




