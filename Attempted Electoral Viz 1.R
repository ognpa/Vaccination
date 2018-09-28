## attempting to visualise data with % of total rather than count
political_party_df_2 <- ggplot(data=elec_imm_1yo_51) + geom_bar(aes(x = elec_imm_1yo_51$`pc_immun`,y=((..count..)/sum(..count..)))) + scale_y_continuous(labels = scales::percent())+ theme(axis.text.x = element_text(size=6,angle = 90, hjust = 1)) + facet_wrap(elec_imm_1yo_51$PartyNm2016~elec_imm_1yo_51$year) + labs(y="% of total", x="Percent immunized",title="Party elected in 2016 and vaccination")

##error message
Error in number(x = x, accuracy = accuracy, scale = scale, prefix = prefix,  : 
                  argument "x" is missing, with no default