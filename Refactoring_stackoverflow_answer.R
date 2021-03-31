# my data
CA <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
Xb_exp <- c(0.0231,0.0519,0.0839,0.1197,0.1595,0.1996,0.2384,0.2772,0.3153,0.3520,0.3887,0.4254,0.4615,0.4978,0.5339,0.5685,0.6000,0.6279,0.6528,0.6762,0.6974,0.7166,0.7346,0.7516,0.7669,0.7810,0.7940,0.8059)
Xb_dw <- c(0.0160,0.0516,0.0886,0.1259,0.1633,0.2006,0.2377,0.2749,0.3122,0.3496,0.3870,0.4245,0.4617,0.4984,0.5339,0.5678,0.5996,0.6288,0.6551,0.6786,0.6994,0.7179,0.7346,0.7499,0.7641,0.7774,0.7899,0.8018)
Xb_f <- c(0.0021,0.0031,0.0046,0.0067,0.0095,0.0131,0.0177,0.0234,0.0387,0.0483,0.0591,0.0709,0.0832,0.0955,0.1073,0.1181,0.1272,0.1345,0.1398,0.1443,0.1456,0.1468,0.1474,0.1476,0.1477,0.1477,0.1477,0.1477)
Xb_s <- c(0.0139,0.0484,0.0839,0.1192,0.1538,0.1874,0.2200,0.2515,0.2818,0.3108,0.3387,0.3653,0.3908,0.4151,0.4383,0.4604,0.4815,0.5015,0.5206,0.5387,0.5559,0.5722,0.5877,0.6024,0.6164,0.6264,0.6421,0.6040)

# dat <- c(CA, Xb_exp, Xb_dw, Xb_f, Xb_s)

dat <- data.frame(CA, Xb_exp, Xb_dw, Xb_f, Xb_s)

# my code

labels =  c(expression(X[b_exp]),expression(X[b_dw]),expression(X[b_f]),expression(X[b_s]))
color4 <- c("Xb_exp"="#3C5488FF", "Xb_dw"="#DC0000FF", "Xb_f"="#00A087FF", "Xb_s"="#4DBBD5FF")
Xb_D1 <- ggplot(data = dat) +
  theme_bw() +
  labs(x="Crank position (ºCA)", y= bquote('Burn fraction ('~X[b]~')')) +
  geom_point(aes(x=CA, y=Xb_exp, colour="Xb_exp"), size=3) +
  geom_line(aes(x=CA, y=Xb_dw,colour="Xb_dw"), size=1,linetype="solid") +
  geom_line(aes(x=CA, y=Xb_f,colour="Xb_f"), size=1,linetype="dotted") +
  geom_line(aes(x=CA, y=Xb_s,colour="Xb_s"), size=1,linetype="longdash") +
  scale_colour_manual(values=color4, labels=labels) +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.5), 
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(2,80))
plot(Xb_D1)

dat %>% 
  pivot_longer(Xb_exp:Xb_s) %>%
  ggplot(aes(x = CA, y = value, colour = name)) +
  geom_point() + 
  geom_line() +
  scale_colour_manual(values=color4, labels=labels) +
  theme_bw() +
  theme(legend.title = element_blank(),legend.position = c(0.8, 0.5),
  legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(2,80)) + 
  labs(x="Crank position (ºCA)", y= bquote('Burn fraction ('~X[b]~')')) 

?bquote
