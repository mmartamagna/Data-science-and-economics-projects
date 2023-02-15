***ANALISI STATISTICHE INDIVIDUALI 09/11/2022**

*Encode variabili categor**
encode azienda, gen(code_azienda)
encode genere, gen(code_genere)
encode classe_età, gen(code_età)
encode anzianità_servizio, gen(code_anzianità)
encode ruolo, gen(code_ruolo)
encode dimensione_azienda, gen(code_dimensione)


**Statistiche descrittive generali**
tab code_anzianità
tab code_ruolo
tab code_genere
tab code_azienda
tab code_dimensione
tab code_genere code_dimensione, r
tab code_ruolo code_età, r
tab code_ruolo code_dimensione, r
tab code_ruolo code_anzianità, r
tab code_ruolo code_genere, r
tab code_ruolo code_genere if code_dimensione == 1, r
tab code_ruolo code_genere if code_dimensione == 2, r
tab code_ruolo code_genere if code_dimensione == 3, r
tab code_ruolo code_genere if code_dimensione == 4, r


**2. PUNTEGGI BWL, LIVELLO PARTECIPAZIONE, BES, DIMENSIONI SALUTE ORGANIZZATIVA, DOMINI BES, ITEMS PARTECIPAZIONE**
ci means BWL livello_partecipazione BES_Lavoratori_P
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 1
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 2
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 3
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 4

ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 1 & code_genere == 4
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 1 & code_genere == 5
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 2 & code_genere == 4
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 2 & code_genere == 5
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 3 & code_genere == 4
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 3 & code_genere == 5
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 4 & code_genere == 4
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 4 & code_genere == 5
ci means BWL livello_partecipazione BES_Lavoratori_P if code_dimensione == 4 & code_genere == 6

ci means BWL livello_partecipazione BES_Lavoratori_P if code_ruolo == 1
ci means BWL livello_partecipazione BES_Lavoratori_P if code_ruolo == 2
ci means BWL livello_partecipazione BES_Lavoratori_P if code_ruolo == 3
ci means BWL livello_partecipazione BES_Lavoratori_P if code_ruolo == 4

**GRAFICI**
graph bar (mean) BWL (mean) BES_Lavoratori_P (mean) livello_partecipazione, over(code_dimensione)  blabel(bar, format(%9.3f)) ytitle("Punteggio medio") title("Punteggi medi BWL, BES, Livello Partecipazione") subtitle("in base alla categoria dimensionale", margin(b=8)) note ("Fonte: elaborazione propria") ysize(9) xsize(12)

graph bar (mean) BWL (mean) BES_Lavoratori_P (mean) livello_partecipazione, over(code_ruolo)  blabel(bar, format(%9.3f)) ytitle("Punteggio medio") title("Punteggi medi BWL, BES, Livello Partecipazione") subtitle("in base al ruolo ricoperto", margin(b=8)) note ("Fonte: elaborazione propria") ysize(9) xsize(12)

**GRAFICI**
cibar BWL, over(code_ruolo)
cibar BWL, over(code_età)
cibar BWL, over(code_anzianità)
cibar BWL, over(code_genere)

cibar livello_partecipazione, over(code_ruolo)
cibar livello_partecipazione, over(code_età)
cibar livello_partecipazione, over(code_anzianità)
cibar livello_partecipazione, over(code_genere)

cibar BES, over(code_ruolo)
cibar BES, over(code_età)
cibar BES, over(code_anzianità)
cibar BES, over(code_genere)

**DIMENSIONI DELLA SALUTE ORGANIZZATIVA**
ci means Ambiente_di_Lavoro Obiettivi_chiari_e_Coerenza Valorizzazione_Competenze Ascolto_Attivo Com_e_Cond_Informazioni Gestione_delle_Conflittualità Ambiente_Relazionale Scorrevolezza_Amministrativa Equità Livello_Stress Ricchezza_di_Senso_del_Lavoro Prevenzione_Infortuni Gest_Lav_Sost_Impegno Apertura_Innovazione Welfare_Organizzativo

ci means Ambiente_di_Lavoro Obiettivi_chiari_e_Coerenza Valorizzazione_Competenze Ascolto_Attivo Com_e_Cond_Informazioni Gestione_delle_Conflittualità Ambiente_Relazionale Scorrevolezza_Amministrativa Equità Livello_Stress Ricchezza_di_Senso_del_Lavoro Prevenzione_Infortuni Gest_Lav_Sost_Impegno Apertura_Innovazione Welfare_Organizzativo if code_dimensione == 1

ci means Ambiente_di_Lavoro Obiettivi_chiari_e_Coerenza Valorizzazione_Competenze Ascolto_Attivo Com_e_Cond_Informazioni Gestione_delle_Conflittualità Ambiente_Relazionale Scorrevolezza_Amministrativa Equità Livello_Stress Ricchezza_di_Senso_del_Lavoro Prevenzione_Infortuni Gest_Lav_Sost_Impegno Apertura_Innovazione Welfare_Organizzativo if code_dimensione == 2

ci means Ambiente_di_Lavoro Obiettivi_chiari_e_Coerenza Valorizzazione_Competenze Ascolto_Attivo Com_e_Cond_Informazioni Gestione_delle_Conflittualità Ambiente_Relazionale Scorrevolezza_Amministrativa Equità Livello_Stress Ricchezza_di_Senso_del_Lavoro Prevenzione_Infortuni Gest_Lav_Sost_Impegno Apertura_Innovazione Welfare_Organizzativo if code_dimensione == 3

ci means Ambiente_di_Lavoro Obiettivi_chiari_e_Coerenza Valorizzazione_Competenze Ascolto_Attivo Com_e_Cond_Informazioni Gestione_delle_Conflittualità Ambiente_Relazionale Scorrevolezza_Amministrativa Equità Livello_Stress Ricchezza_di_Senso_del_Lavoro Prevenzione_Infortuni Gest_Lav_Sost_Impegno Apertura_Innovazione Welfare_Organizzativo if code_dimensione == 4

**12 DOMINI DEL BES**
ci means Salute Istruzione_Formazione Lavoro Benessere_Economico Relazioni_Sociali Politica_Istituzioni_Org Sicurezza Benessere_Soggettivo Paesaggio_Patrimonio_Culturale Ambiente Innovazione_Ric_Creat Qualità_Servizi if code_dimensione == 1

ci means Salute Istruzione_Formazione Lavoro Benessere_Economico Relazioni_Sociali Politica_Istituzioni_Org Sicurezza Benessere_Soggettivo Paesaggio_Patrimonio_Culturale Ambiente Innovazione_Ric_Creat Qualità_Servizi if code_dimensione == 2

ci means Salute Istruzione_Formazione Lavoro Benessere_Economico Relazioni_Sociali Politica_Istituzioni_Org Sicurezza Benessere_Soggettivo Paesaggio_Patrimonio_Culturale Ambiente Innovazione_Ric_Creat Qualità_Servizi if code_dimensione == 3

ci means Salute Istruzione_Formazione Lavoro Benessere_Economico Relazioni_Sociali Politica_Istituzioni_Org Sicurezza Benessere_Soggettivo Paesaggio_Patrimonio_Culturale Ambiente Innovazione_Ric_Creat Qualità_Servizi if code_dimensione == 4

**ITEMS DELLA PARTECIPAZIONE**
ci means welfare sviluppo_organizzazione gestione_hr organizzazione_lavoro remunerazione_premi_benefit inf_com_interna inf_com_esterna formazione_competenze salute_sicurezza situazione_eco_fin_bilancio selezione_fornitori innovazione_prodotto_processo investimenti occupazione pianificazione_generale pianificazione_area_appartenenza azionariato_lavoratori prob_conflitti_controv_interne prob_conflitti_controv_esterne stakeholder_engagement sost_ambientale if code_dimensione == 1

ci means welfare sviluppo_organizzazione gestione_hr organizzazione_lavoro remunerazione_premi_benefit inf_com_interna inf_com_esterna formazione_competenze salute_sicurezza situazione_eco_fin_bilancio selezione_fornitori innovazione_prodotto_processo investimenti occupazione pianificazione_generale pianificazione_area_appartenenza azionariato_lavoratori prob_conflitti_controv_interne prob_conflitti_controv_esterne stakeholder_engagement sost_ambientale if code_dimensione == 2

ci means welfare sviluppo_organizzazione gestione_hr organizzazione_lavoro remunerazione_premi_benefit inf_com_interna inf_com_esterna formazione_competenze salute_sicurezza situazione_eco_fin_bilancio selezione_fornitori innovazione_prodotto_processo investimenti occupazione pianificazione_generale pianificazione_area_appartenenza azionariato_lavoratori prob_conflitti_controv_interne prob_conflitti_controv_esterne stakeholder_engagement sost_ambientale if code_dimensione == 3

ci means welfare sviluppo_organizzazione gestione_hr organizzazione_lavoro remunerazione_premi_benefit inf_com_interna inf_com_esterna formazione_competenze salute_sicurezza situazione_eco_fin_bilancio selezione_fornitori innovazione_prodotto_processo investimenti occupazione pianificazione_generale pianificazione_area_appartenenza azionariato_lavoratori prob_conflitti_controv_interne prob_conflitti_controv_esterne stakeholder_engagement sost_ambientale if code_dimensione == 4



mean BES, over(code_età)

**3. CORRELAZIONI**
pwcorr livello_partecipazione BWL BES_Lavoratori_P, star(.05)

*In base alla dimensione
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_dimensione == 1, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_dimensione == 2, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_dimensione == 3, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_dimensione == 4, star(.05)

*In base al genere
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_genere == 4, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_genere == 5, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_genere == 6, star(.05)

*In base al ruolo
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_ruolo == 1, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_ruolo == 2, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_ruolo == 3, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_ruolo == 4, star(.05)

*In base all'anzianità_servizio
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_anzianità == 1, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_anzianità  == 2, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_anzianità  == 3, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_anzianità  == 4, star(.05)

* In base all'età
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_età == 1, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_età == 2, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_età  == 3, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_età == 4, star(.05)
pwcorr livello_partecipazione BWL BES_Lavoratori_P if code_età == 5, star(.05)


**DOMINI BENESSERE SOGGETTIVO (media totale, media impiegati, media responsabili/referenti)**
ci means Sodd_per_Organizzazione Parte_di_un_team Autorealizzazione Convinzione_cambiamento Work_Life_Balance Relazioni_Interpersonali Credibilità_Man Stima_Man Valori_org_condivisi Percezione_Successo_Org Voglia_Impegno Voglia_Andare_Lavoro

**DOMINI MALESSERE SOGGETTIVO (media totale, media impiegati, media responsabili/referenti)**
ci means Assenza_propositività Assenteismo Confusione_organizzativa Lentezza_prestazione Anaffettività_Lavorativa Pettegolezzo Desiderio_cambiare_lavoro Sentimento_Disconoscimento Sentimento_Irrilevanza Sentimento_Inutilità Nervosismo Disinteresse_Lavoro Risentimento

**DIMENSIONI DELLO STRESS**
mean Relazioni Controllo Supporto_Man Supp_Colleghi Cambiamento Ruolo Domanda

** TUTTE E 127 DOMANDE **
i_12_1_p_politica_domanda_b11 i_12_2_p_sicurezza_supp_man i_12_3_p_sicurezza i_12_4_n_salute_relazioni_b6 i_12_5_p_relazioni_supp_man_b11 i_1_1_p_salute i_1_2_p_sicurezza i_1_3_p_salute_supp_man i_1_4_p_lavoro_supp_man i_1_5_n_salute i_1_6_p_lavoro_supp_man i_1_7_p_lavoro_supp_man i_1_8_p_paesaggio i_1_9_p_paesaggio i_1_10_p_qservizi_supp_man i_4_1_p_relazioni i_4_2_p_politica_relazioni_m13 i_4_3_p_bsoggettivo_supp_man_b5 i_4_4_p_politica i_4_5_p_qservizi_supp_man_b11 i_4_6_p_politica_b11 i_4_7_p_relazioni_relazioni_b8 i_4_8_p_lavoro_m7 i_9_1_p_beco_ruolo_m5 i_9_2_p_lavoro_ruolo i_9_3_p_beco_ruolo_m5 i_9_4_p_lavoro_ruolo_m12 i_9_5_n_lavoro_m1 i_9_6_n_politica_b11 i_8_1_p_lavoro_supp_man_b10 i_8_2_p_lavoro_controllo_m11 i_8_3_p_lavoro_supp_man_b6 i_8_4_p_lavoro i_8_5_p_lavoro_supp_coll_m11 i_8_6_p_politica_b3 i_8_7_p_relazioni_supp_coll_b3 i_8_8_n_relazioni_supp_man_b12 i_13_1_p_lavoro_domanda_b7 i_13_2_n_lavoro_domanda_b7 i_13_3_n_relazioni_supp_coll_b3 i_13_4_p_lavoro_b10 i_13_5_p_relazioni_domanda_b10 i_13_6_p_lavoro_domanda i_13_7_p_relazioni_domanda_b10 i_13_8_n_lavoro_domanda_m12 i_13_9_p_lavoro_domanda_m12 i_14_1_p_innovazione_cambiamento i_14_2_p_istruzione_b5 i_14_3_p_lavoro_supp_man i_14_4_p_innovazione i_14_5_n_innovazione i_14_6_p_lavoro_controllo_b7 i_14_7_p_bsoggettivo_b12 i_6_1_p_relazioni_supp_coll_b3 i_6_2_n_bsoggettivo_m2 i_6_3_p_relazioni i_6_4_p_bsoggettivo_supp_coll_b3 i_6_5_n_bsoggettivo_supp_coll i_6_6_p_lavoro_supp_man_b11 i_6_7_n_relazioni_relazioni_b8 i_6_8_n_relazioni_relazioni_b8 i_6_9_n_sicurezza_relazioni_b8 i_6_10_n_lavoro_supp_man_m1 i_6_11_n_lavoro_supp_coll_b8 i_15_2_n_lavoro_m14 i_10_1_p_bsoggettivo i_10_2_n_salute_domanda_b7 i_10_3_n_salute_domanda i_10_4_n_salute_b7 i_10_5_n_salute_b7 i_10_6_n_lavoro_domanda_b7 i_10_8_p_politica_supp_man_b11 i_10_9_p_qservizi i_10_12_p_bsoggettivo i_10_13_p_lavoro_controllo_b7 i_10_14_p_salute_cotrollo i_10_15_n_bsoggettivo_b7 i_5_1_p_politica_cambiamento_b9 i_5_2_p_politica_cambiamento_b9 i_5_3_p_politica_cambiamento_b9 i_5_4_p_politica_b10 i_2_1_p_lavoro_ruolo i_2_2_p_politica i_2_3_p_politica_ruolo i_2_4_n_relazioni_relazioni_m4 i_2_5_n_relazioni_relazioni_m4 i_2_6_n_lavoro_ruolo_m12 i_2_7_n_lavoro_ruolo_m10 i_2_8_n_beco_supp_man_m1 i_3_1_p_bsoggettivo_supp_collegh i_3_2_p_bsoggettivo_supp_mang_m5 i_3_3_p_lavoro_domanda_m3 i_3_4_p_relazioni i_3_5_p_relazioni_relazioni_b8 i_3_6_p_relazioni_domanda_m3 i_3_7_p_beco_ruolo_m5 i_3_8_p_beco_ruolo_m5 i_3_9_p_relazioni_supp_coll i_3_10_p_relazioni_supp_coll_b8 i_3_11_p_istruzione_controllo i_3_12_p_relazioni_relazioni_b8 i_3_13_p_relazioni_relazioni_m13 i_15_1_p_lavoro_b1 i_15_3_n_lavoro_m8 i_11_1_p_lavoro_ruolo_b5 i_11_2_p_bsoggettivo i_11_3_p_lavoro_b1 i_11_4_p_ambiente_b5 i_11_5_p_lavoro_m4 i_11_6_p_lavoro_b4 i_11_7_p_bsoggettivo_m7 i_11_8_p_lavoro_b2 i_11_9_p_ambiente i_15_4_n_relazioni_relazioni_m9 i_7_1_p_politica_domanda i_7_2_p_lavoro_controllo_b9 i_7_3_p_politica i_7_4_p_relazioni_relazioni_b8 i_7_5_n_relazioni_relazioni_m9 i_10_7_p_qservizi i_10_10_p_istruzione i_10_11_p_qservizi i_16_1_p_beco i_16_2_p_beco i_16_3_p_beco i_16_4_p_bsoggettivo i_16_5_p_bsoggettivo