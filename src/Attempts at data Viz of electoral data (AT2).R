## Attempting to create a viz out of the electoral & postcode data

ggplot(elec_imm_2016_majority, mapping = aes(x = PartyAb, y = pc_immun, color = age, size = `Per cent``)) + geom_point())
ggplot(elec_imm_2016_majority, aes(x = PartyAb, y = pc_immun, color = age, size = `Per cent`)) + geom_point()
ggplot(elec_imm_2016_majority, aes(x = age, y = pc_immun, color = PartyAb, size = `Per cent`)) + geom_point()

LP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb2016 == "LP")
LP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb == "LP")
LP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb2016 == "LP")
LP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb == "LP")
ALP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb == "ALP")
NP_stronghold <- elec_imm_2016_majority %>% filter(PartyAb == "NP")
GRN_stronghold <- elec_imm_2016_majority %>% filter(PartyAb == "GRN")

by_party_viz <-elec_imm_2016_majority %>% group_by(`Electoral division`, PartyAb)
electorate_immunisation_join <- elec_result_PC_all %>% left_join(immunisation_data, by = c("Postcode" = "postcode"))

View(electorate_immunisation_join)
View(elec_imm_2016_majority)

Postcode_majority <- Immunization_with_everything %>% select(postcode, year, Time, PartyAb2016, PartyAb2010, PartyAb2013, pc_immun, pc_immun_class, Electoral.division, Per.cent.postcode.in.electorate) %>% filter(Per.cent.postcode.in.electorate >= 51)
View(Postcode_majority)
Stronghold viz <- Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013== "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = PartyAb2016, y = year)) + geom_point()
Stronghold_viz <- Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013 == "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = PartyAb2016, y = year)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013 == "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = PartyAb2016, y = year)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013 == "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = PartyAb2016, y = PartyAb2010)) + geom_point()
View(Postcode_majority)
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013 == "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = PartyAb2016, y = pc_immun)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% filter(PartyAb2016 == "GRN", PartyAb2013 == "GRN", PartyAb2010 == "GRN") %>% filter(PartyAb2016 == "NP", PartyAb2013 == "NP", PartyAb2010 == "NP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = year)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = year)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = year, y = pc_immun)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = year, y = PartyAb2016)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = PartyAb2016)) + geom_point()
Parties <- c(PartyAb2016, PartyAb2013, PartyAb2010)
Parties <- c("PartyAb2016", "PartyAb2013", "PartyAb2010")
Parties

Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = Parties)) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = 'Parties')) + geom_point()
Postcode_majority %>% filter(PartyAb2016 == "ALP", PartyAb2013 == "ALP", PartyAb2010 == "ALP") %>% ggplot(Postcode_majority, mapping = aes(x = pc_immun, y = 'Parties')) + geom_point()
Party_strongholds <- c("LP_stronghold", "ALP_stronghold", "GRN_stronghold", "NP_stronghold")
ggplot(Postcode_majority, mapping = aes(x = "Party_strongholds", y = pc_immun))
ggplot(Postcode_majority, mapping = aes(x = "Party_strongholds", y = pc_immun)) + geom_smooth()
ggplot(Postcode_majority, mapping = aes(x = Electoral.division, y = pc_immun)) + geom_smooth()
ggplot(Postcode_majority, mapping = aes(x = Electoral.division, y = pc_immun_class)) + geom_smooth()
ggplot(Postcode_majority, mapping = aes(x = Party_strongholds, y = pc_immun_class)) + geom_smooth()

political_party_df=ggplot(data=d2_dash, aes(d2_dash$`Percent fully immunised (%)`)) + geom_histogram(stat='count') + theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_grid(d2_dash$variable~d2_dash$value)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df=ggplot(data=d2_dash, aes(d2_dash$`Percent fully immunised (%)`)) + geom_histogram(stat='count') + theme(axis.text.x = element_text(size= ,angle = 90, hjust = 1)) + facet_grid(d2_dash$variable~d2_dash$value)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

elec_imm_1yo <- electorate_immunisation_join %>% filter(age == 1)
elec_imm_1yo_51 <- electorate_immunisation_join %>% filter(age == 1) %>% filter(Per.cent.postcode.in.electorate >= 51)
View(elec_imm_1yo)
elec_imm_1yo_51 <- electorate_immunisation_join %>% filter(age == 1) %>% filter(`Per cent` >= 51)
View(elec_imm_1yo_51)

political_party_df=ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`) + geom_histogram(stat='count') + theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_grid(elec_imm_1yo_51$variable~elec_imm_1yo_51$value)+ labs(y="Count",x= "pc_immun"",title="Vaccination rate of 1 y/olds by political party")

Lib_stronghold <- elec_imm_1yo_51 %>% filter(PartyAb2016 == "LP", PartyAb2013 == "LP", PartyAb2010 == "LP")                                                   View(Lib_stronghold)
                                                                                                                                                              library(gapminder)
                                                                                                                                                              library(dplyr)
                                                                                                                                                              library(ggplot2)
                                                                                                                                                              # Trying to do a Scatter plot comparing gdpPercap and lifeExp, with color representing continent and size representing population, faceted by year

ggplot (gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + geom_point () +scale_x_log10 () +facet_wrap (~year)
                                                                                                                                                              ggplot(data = Lib_stronghold, aes(x = year, y = pc_immun))
                                                                                                                                                              ggplot(data = Lib_stronghold, aes(x = year, y = pc_immun)) + geom_point()
                                                                                                                                                              ggplot(data = Lib_stronghold, aes(x = year, y = pc_immun)) + geom_histogram()
                                                                                                                                                              ggplot(data = Lib_stronghold, aes(x = year, y = pc_immun)) + geom_smooth()
                                                                                                                                                              ggplot(data = Lib_stronghold, aes(x = year, y = pc_immun)) + geom_point()
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(x = PartyAb2016, y = pc_immun)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(x = PartyAb2016$count, y = pc_immun)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(x = count(PartyAb2016), y = pc_immun)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(x = count(elec_imm_1yo_51$PartyAb2016), y = pc_immun)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(x = count(elec_imm_1yo_51,PartyAb2016), y = pc_immun)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(y = pc_immun)) + geom_bar(mapping = NULL, data = NULL, stat = count(PartyAb2016)) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(y = pc_immun)) + geom_bar(mapping = NULL, data = NULL, stat = count("PartyAb2016")) + facet_wrap(~year)
                                                                                                                                                              ggplot(data = elec_imm_1yo_51, aes(, x = year, y = pc_immun)) + geom_bar(mapping = NULL, data = NULL, stat = "count) + facet_wrap(~PartyAb2016)
                                                                                                                                                                                                                                                    )
ggplot(data = elec_imm_1yo_51, aes(, x = year, y = pc_immun)) + geom_bar(mapping = NULL, data = NULL, stat = "count") + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_bar(mapping = NULL, data = NULL, stat = "count") + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_bar() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_point() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_histogram() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_histogram(stat = "count") + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_col() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun_class)) + geom_col() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun_class)) + geom_col() + facet_wrap(~PartyAb2016) %>% filter(PartyAb2016 == "ALP", PartyAb2016 == "LP)

elec_imm_1yo_51 %>% filter(PartyAb2016 == "ALP", PartyAb2016 == "LP") + ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun_class)) + geom_col() + facet_wrap(~PartyAb2016)

elec_imm_1yo_51_2P %>% filter(PartyAb2016 == "ALP", PartyAb2016 == "LP")
elec_imm_1yo_51_2P <- elec_imm_1yo_51 %>% filter(PartyAb2016 == "ALP", PartyAb2016 == "LP")
ggplot(data = elec_imm_1yo_51_2P, aes(x = year, y = pc_immun_class)) + geom_col() + facet_wrap(~PartyAb2016)


View(elec_imm_1yo_51_2P)
View(elec_imm_1yo_51)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun_class)) + geom_col() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51_2P, aes(x = year, y = pc_immun)) + geom_col() + facet_wrap(~PartyAb2016)
ggplot(data = elec_imm_1yo_51, aes(x = year, y = pc_immun)) + geom_col() + facet_wrap(~PartyAb2016)
elec_imm_1yo_51_2 = unique(elec_imm_1yo_51[,c('Postcode', 'PartyAb2016', 'PartyAb2013','PartyAb2010', 'pc_immun')])

View(elec_imm_1yo_51)
View(elec_imm_1yo_51_2)

elec_imm_1yo_51_2 = unique(elec_imm_1yo_51[,c('Postcode', 'PartyAb2016', 'PartyAb2013','PartyAb2010', 'pc_immun', 'year')])
View(elec_imm_1yo_51_2)

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(elec_imm_1yo_51$variable~elec_imm_1yo_51$value)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(~elec_imm_1yo_51$value)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(~'year')+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(~elec_imm_1yo_51$StateAB2016)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(~elec_imm_1yo_51$PartyNm2016)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_grid(~elec_imm_1yo_51$PartyNm2016)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination") + facet_wrap(~'year')
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(~elec_imm_1yo_51$PartyNm2016, ~'year')+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(~elec_imm_1yo_51$PartyNm2016)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
po
political_party_df
political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$year~elec_imm_1yo_51$PartyNm2016)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(formatter = percent)+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(formatter = 'percent')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(formatter = "percent")+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(formatter = 'percent')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(formatter = `percent`)+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(x = elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(x = elec_imm_1yo_51$pc_immun)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(elec_imm_1yo_51, aes(x = `pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(elec_imm_1yo_51, aes(x = `pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(elec_imm_1yo_51, aes(x = `pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(~elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 = ggplot(elec_imm_1yo_51, aes(x = `pc_immun`, y = elec_imm_1yo_51$PartyNm2016)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(~elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + filter %>% year(2010, 2013, 2016) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + %>% filter(year == 2010, year == 2013, year == 2016) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_3 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + %>% filter(year == 2010, year == 2013, year == 2016) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~.)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_3 = ggplot(data=elec_imm_1yo_51, aes(elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~.)+  labs(y="Count",x="Percent immunized",title="Party elected in 2016 and vaccination")
political_party_df_3

political_party_df_3 = ggplot(data=elec_imm_1yo_51, aes(x = elec_imm_1yo_51$`pc_immun`)) + geom_histogram(stat='count')+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1))+facet_wrap(elec_imm_1yo_51$PartyNm2016~.)+  labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)
political_party_df_3

political_party_df_2 = ggplot(data=elec_imm_1yo_51, aes(x = elec_imm_1yo_51$`pc_immun`)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination")

political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(aes(x = elec_imm_1yo_51$`pc_immun`),y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y=“% of total”,x=“Percent immunized”,title=“Party elected in 2016 and vaccination”)

political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(aes(x = elec_imm_1yo_51$`pc_immun`),y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)

political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(aes(x = elec_imm_1yo_51$`pc_immun`),y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination))

political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`),y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)
                                                                                                                                                                                                                                                                                                                           political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`),(y=(..count..)/sum(..count..)) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)
                                                                                                                                                                                                                                                                                                                           )
political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`),y=(..count..)/sum(..count..)) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)
                                                                                                                                                                                                                                                                                                                          political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`,y=(..count..)/sum(..count..)) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)

political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`,y=(..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)
                                                                                                                                                                                                                                                                                                                          political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`,y=((..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total",x="Percent immunized",title="Party elected in 2016 and vaccination)

