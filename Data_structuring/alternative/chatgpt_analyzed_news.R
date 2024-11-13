# Create the data frame
shock_data <- data.frame(
  Date = as.Date(c("2015-01-16", "2015-01-21", "2015-01-25", "2015-02-13", "2015-02-19", 
                   "2015-02-24", "2015-03-07", "2015-03-15", "2015-04-01", "2015-06-16", 
                   "2015-07-01", "2015-10-12", "2015-11-24")),
  Shock = c("Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "No", 
            "Yes", "Yes", "Yes"),
  Summary = c("Multiple gas pipeline incidents reported on the same day, including a blast in Delhi and another pipeline blown up in Dera Bugti. These incidents highlight potential disruptions in gas supply due to infrastructure damage.",
              "Russia continues its plan for a gas pipeline to Turkey, indicating ongoing efforts to strengthen gas export routes amid European tensions.",
              "Ongoing geopolitical disputes involving Russia and Ukraine regarding gas payments, with threats of supply cut-offs. Tensions escalate as the EU plans to loosen Russian control over gas supplies.",
              "A gas pipeline blown up in Dera Bugti again, raising concerns over consistent sabotage impacting regional gas supplies in Pakistan.",
              "Europe sees a surge in LNG supply after Asian premiums rise, suggesting a shift in global LNG flows driven by market demand.",
              "The EU takes steps to form an energy union to reduce dependence on Russian gas. Meanwhile, Putin accuses Ukraine of genocide over gas cut-offs, which further strains bilateral ties.",
              "Nigeria suffers significant economic losses due to pipeline vandalism, highlighting risks to domestic gas supplies and potential revenue impacts.",
              "A new gas pipeline project in the Gulf of Mexico is expected to generate economic growth and job creation, signaling investment in infrastructure.",
              "Russia halts gas supplies to Ukraine following breakdowns in talks, raising concerns over Europe's energy security given the reliance on Russian pipelines.",
              "Discoveries and projects in the gas sector continue, with Iran planning a pipeline to Iraq and the EU exploring alternatives to reduce Russian influence.",
              "Russia officially cuts gas supplies to Ukraine due to unresolved pricing disputes, exacerbating regional energy tensions.",
              "Nigeria announces gas supply cuts to Ghana due to debt issues, which could impact electricity generation in Ghana, demonstrating the interconnectedness of regional energy markets.",
              "Crimea faces potential gas supply cuts amid tensions with Russia, which could lead to a humanitarian crisis if prolonged."
  )
)

# View the data frame
print(shock_data)
