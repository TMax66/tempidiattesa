
queryTaut <- "SELECT        dbo.Conferimenti.Numero AS numero, dbo.Anag_Registri.Descrizione AS settore, dbo_Anag_Finalita_Confer.Descrizione AS finalita, dbo.Anag_Finalita.Descrizione AS finprova, dbo.Conferimenti.Data_Prelievo AS dtprel, 
                         dbo.Conferimenti.Data AS dtconf, dbo.Conferimenti.Data_Accettazione AS dtreg, dbo.Conferimenti.Data_Inserimento AS dtprog, dbo.Esami_Aggregati.Data_Invio AS dtinvio, dbo.Esami_Aggregati.Data_Carico AS dtcarico, 
                         dbo.Esami_Aggregati.Data_Inizio_Analisi AS dtinizio, dbo.Esami_Aggregati.Data_Fine_Analisi AS dtfine, dbo.Esami_Aggregati.Codice AS nprogram, dbo.Anag_Gruppo_Prove.Descrizione AS categprove, 
                         dbo_Anag_Prove_Gruppi.Descrizione AS gruppo, dbo.Anag_Prove.Descrizione AS prova, dbo.Anag_Metodi_di_Prova.Descrizione AS mp, dbo_Anag_Reparti_ConfAcc.Descrizione AS repacc, 
                         dbo_Anag_Reparti_ConfProp.Descrizione AS repprop, dbo.Anag_Reparti.Descrizione AS repanalisi, dbo.Anag_Laboratori.Descrizione AS labanalisi, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato AS primoRdp, 
                         dbo.Esami_Aggregati.Tot_Eseguiti AS nesami, dbo.Anag_TipoConf.Descrizione AS tipopag, dbo.Anag_Tipo_Prel.Descrizione AS tipoprel, dbo.Conferimenti.NrCampioni, dbo.Anag_Comuni.Descrizione AS Comune, 
                         dbo.Anag_Comuni.Provincia, dbo.Anag_Comuni.Regione, dbo.Anag_Comuni.codice_istat, dbo.Anag_Tecniche.Descrizione AS Tecnica, dbo.Nomenclatore.Codice_Gruppo, dbo.Nomenclatore.Codice_Prova, 
                         dbo.Nomenclatore.Codice_Tecnica, dbo.Nomenclatore.Chiave, dbo_Nomenclatore_Gruppi.Codice_Gruppo AS Codgruppo, 
						 dbo_Nomenclatore_Gruppi.Codice_Prova AS COdprovaNMgruppi, dbo_Nomenclatore_Gruppi.Codice_Tecnica AS CodTecnicaNMgruppi, 
                         dbo_Nomenclatore_Gruppi.Chiave AS ChiaveGruppi
FROM            dbo.Anag_Registri INNER JOIN
                         dbo.Conferimenti ON dbo.Conferimenti.Registro = dbo.Anag_Registri.Codice LEFT OUTER JOIN
                         dbo.Esami_Aggregati ON dbo.Conferimenti.Anno = dbo.Esami_Aggregati.Anno_Conferimento AND dbo.Conferimenti.Numero = dbo.Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
                         dbo.Nomenclatore_MP ON dbo.Esami_Aggregati.Nomenclatore = dbo.Nomenclatore_MP.Codice LEFT OUTER JOIN
                         dbo.Anag_Metodi_di_Prova ON dbo.Nomenclatore_MP.MP = dbo.Anag_Metodi_di_Prova.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Settori ON dbo.Nomenclatore_MP.Nomenclatore_Settore = dbo.Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore ON dbo.Nomenclatore_Settori.Codice_Nomenclatore = dbo.Nomenclatore.Chiave LEFT OUTER JOIN
                         dbo.Anag_Prove ON dbo.Nomenclatore.Codice_Prova = dbo.Anag_Prove.Codice LEFT OUTER JOIN
                         dbo.Anag_Gruppo_Prove ON dbo.Nomenclatore.Codice_Gruppo = dbo.Anag_Gruppo_Prove.Codice LEFT OUTER JOIN
                         dbo.Programmazione_Finalita ON dbo.Esami_Aggregati.Anno_Conferimento = dbo.Programmazione_Finalita.Anno_Conferimento AND 
                         dbo.Esami_Aggregati.Numero_Conferimento = dbo.Programmazione_Finalita.Numero_Conferimento AND dbo.Esami_Aggregati.Codice = dbo.Programmazione_Finalita.Codice LEFT OUTER JOIN
                         dbo.Anag_Finalita ON dbo.Programmazione_Finalita.Finalita = dbo.Anag_Finalita.Codice LEFT OUTER JOIN
                         dbo.Laboratori_Reparto ON dbo.Esami_Aggregati.RepLab_analisi = dbo.Laboratori_Reparto.Chiave LEFT OUTER JOIN
                         dbo.Anag_Reparti ON dbo.Laboratori_Reparto.Reparto = dbo.Anag_Reparti.Codice LEFT OUTER JOIN
                         dbo.Anag_Laboratori ON dbo.Laboratori_Reparto.Laboratorio = dbo.Anag_Laboratori.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_MP AS dbo_Nomenclatore_MP_Gruppi ON dbo.Esami_Aggregati.Gruppo_Nomenclatore = dbo_Nomenclatore_MP_Gruppi.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Settori AS dbo_Nomenclatore_Settori_Gruppi ON dbo_Nomenclatore_MP_Gruppi.Nomenclatore_Settore = dbo_Nomenclatore_Settori_Gruppi.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore AS dbo_Nomenclatore_Gruppi ON dbo_Nomenclatore_Settori_Gruppi.Codice_Nomenclatore = dbo_Nomenclatore_Gruppi.Chiave LEFT OUTER JOIN
                         dbo.Anag_Prove AS dbo_Anag_Prove_Gruppi ON dbo_Nomenclatore_Gruppi.Codice_Prova = dbo_Anag_Prove_Gruppi.Codice INNER JOIN
                         dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo.Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave INNER JOIN
                         dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
                         dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON dbo.Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
                         dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
                         dbo.Conferimenti_Finalita ON dbo.Conferimenti.Anno = dbo.Conferimenti_Finalita.Anno AND dbo.Conferimenti.Numero = dbo.Conferimenti_Finalita.Numero INNER JOIN
                         dbo.Anag_Finalita AS dbo_Anag_Finalita_Confer ON dbo.Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice INNER JOIN
                         dbo.Anag_TipoConf ON dbo.Conferimenti.Tipo = dbo.Anag_TipoConf.Codice AND dbo.Esami_Aggregati.Tipo_Pagamento = dbo.Anag_TipoConf.Codice INNER JOIN
                         dbo.Anag_Tipo_Prel ON dbo.Conferimenti.Tipo_Prelievo = dbo.Anag_Tipo_Prel.Codice INNER JOIN
                         dbo.Anag_Comuni ON dbo.Anag_Registri.Codice = dbo.Anag_Comuni.Codice INNER JOIN
                         dbo.Anag_Tecniche ON dbo.Nomenclatore.Codice_Tecnica = dbo.Anag_Tecniche.Codice AND dbo_Nomenclatore_Gruppi.Codice_Tecnica = dbo.Anag_Tecniche.Codice
WHERE        (dbo.Laboratori_Reparto.Laboratorio > 1) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(dbo.Conferimenti.Data_Accettazione) } = 2022) AND 
                         (dbo.Anag_Registri.Descrizione IN ('Alimenti Uomo', 'Alimenti Zootecnici', 'Sanità Animale'))"







# queryTaut <- "SELECT        
# dbo.Conferimenti.Data_Prelievo, 
# dbo.Conferimenti.Data, 
# dbo_Anag_Reparti_ConfAcc.Descrizione AS stracc, 
# dbo_Anag_Reparti_ConfProp.Descrizione AS strapp, 
# dbo.Conferimenti.Numero AS nconf, 
# dbo_Anag_Finalita_Confer.Descrizione AS Finalita, 
# DATENAME(weekday, dbo.Conferimenti.Data_Accettazione) AS giorno, 
# dbo.Conferimenti.Data_Accettazione, dbo.Esami_Aggregati.Codice AS codprogram, 
# dbo.Anag_Prove.Descrizione AS prova, dbo.Esami_Aggregati.Data_Invio, 
# dbo.Esami_Aggregati.Data_Carico, dbo.Anag_Reparti.Descrizione AS repprova, 
# dbo.Anag_Laboratori.Descrizione AS Laboratorio, 
#                          dbo.Esami_Aggregati.Data_Inizio_Analisi, 
#                          dbo.Esami_Aggregati.Data_Fine_Analisi, 
#                          dbo.RDP_Date_Emissione.Data_RDP, 
#                          dbo.Conferimenti.DataOra_Primo_RDP_Completo_Firmato, 
#                          dbo.Conferimenti.NrCampioni, 
#                          dbo.Esami_Aggregati.Tot_Eseguiti, 
#                          dbo.Nomenclatore.Chiave, 
#                          dbo.Anag_Metodi_di_Prova_Base.Descrizione AS mmpp, 
#                          dbo.Anag_Gruppo_Prove.Descrizione AS catprova, 
#                          dbo.Anag_Registri.Descrizione AS reg, 
#                          dbo.Anag_Tipo_Prel.Descrizione AS tipoprelievo, 
#                          dbo.Anag_Sottotipo_Prel.Descrizione AS sottotipoprel, 
#                          dbo.Anag_Sottotipo_Prel_Dett.Descrizione AS stipopreldesc, 
#                          dbo.Anag_Comuni.Descrizione AS Comune, 
#                          dbo.Anag_Comuni.Provincia, 
#                          dbo.Anag_Regioni.Descrizione AS Regione, 
#                          dbo.Conferimenti.Data_Inserimento
# FROM            dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfAcc INNER JOIN
#                          dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
#                          dbo.Conferimenti ON dbo.Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
#                          dbo.Anag_Comuni ON dbo.Anag_Comuni.Codice = dbo.Conferimenti.Luogo_Prelievo LEFT OUTER JOIN
#                          dbo.Anag_Regioni ON dbo.Anag_Regioni.Codice = dbo.Anag_Comuni.Regione LEFT OUTER JOIN
#                          dbo.Esami_Aggregati ON dbo.Conferimenti.Anno = dbo.Esami_Aggregati.Anno_Conferimento AND dbo.Conferimenti.Numero = dbo.Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
#                          dbo.Nomenclatore_MP ON dbo.Esami_Aggregati.Nomenclatore = dbo.Nomenclatore_MP.Codice LEFT OUTER JOIN
#                          dbo.Nomenclatore_Settori ON dbo.Nomenclatore_MP.Nomenclatore_Settore = dbo.Nomenclatore_Settori.Codice LEFT OUTER JOIN
#                          dbo.Nomenclatore ON dbo.Nomenclatore_Settori.Codice_Nomenclatore = dbo.Nomenclatore.Chiave LEFT OUTER JOIN
#                          dbo.Anag_Prove ON dbo.Nomenclatore.Codice_Prova = dbo.Anag_Prove.Codice LEFT OUTER JOIN
#                          dbo.Anag_Gruppo_Prove ON dbo.Nomenclatore.Codice_Gruppo = dbo.Anag_Gruppo_Prove.Codice LEFT OUTER JOIN
#                          dbo.Anag_Metodi_di_Prova_Revisioni ON dbo.Nomenclatore_MP.MP = dbo.Anag_Metodi_di_Prova_Revisioni.Codice LEFT OUTER JOIN
#                          dbo.Anag_Metodi_di_Prova_Base ON dbo.Anag_Metodi_di_Prova_Revisioni.MP_Base = dbo.Anag_Metodi_di_Prova_Base.Codice LEFT OUTER JOIN
#                          dbo.Laboratori_Reparto ON dbo.Esami_Aggregati.RepLab_analisi = dbo.Laboratori_Reparto.Chiave LEFT OUTER JOIN
#                          dbo.Anag_Reparti ON dbo.Laboratori_Reparto.Reparto = dbo.Anag_Reparti.Codice LEFT OUTER JOIN
#                          dbo.Anag_Laboratori ON dbo.Laboratori_Reparto.Laboratorio = dbo.Anag_Laboratori.Codice INNER JOIN
#                          dbo.Anag_Tipo_Prel ON dbo.Conferimenti.Tipo_Prelievo = dbo.Anag_Tipo_Prel.Codice INNER JOIN
#                          dbo.Anag_Registri ON dbo.Conferimenti.Registro = dbo.Anag_Registri.Codice INNER JOIN
#                          dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo.Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave INNER JOIN
#                          dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice LEFT OUTER JOIN
#                          dbo.Anag_Sottotipo_Prel ON dbo.Anag_Sottotipo_Prel.Codice = dbo.Conferimenti.SottoTipo_Prelievo INNER JOIN
#                          dbo.Conferimenti_Finalita ON dbo.Conferimenti.Anno = dbo.Conferimenti_Finalita.Anno AND dbo.Conferimenti.Numero = dbo.Conferimenti_Finalita.Numero INNER JOIN
#                          dbo.Anag_Finalita AS dbo_Anag_Finalita_Confer ON dbo.Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice LEFT OUTER JOIN
#                          dbo.Anag_Sottotipo_Prel_Dett ON dbo.Conferimenti.SottoTipoPrel_Dett = dbo.Anag_Sottotipo_Prel_Dett.Codice LEFT OUTER JOIN
#                          dbo.RDP_Date_Emissione ON dbo.RDP_Date_Emissione.Anno = dbo.Conferimenti.Anno AND dbo.RDP_Date_Emissione.Numero = dbo.Conferimenti.Numero
# WHERE        (dbo.Laboratori_Reparto.Laboratorio > 1) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(dbo.Conferimenti.Data) } = 2022) AND 
#                          (dbo.Anag_Registri.Descrizione NOT IN ('Altri Controlli (cosmetici,ambientali..)', 'Controlli Interni Sistema Qualità')) 
# "
# 
# 

#query con aggiunta della tabella RDP_storico ----

q <- "SELECT        dbo.Conferimenti.Numero AS numero, dbo.Anag_Registri.Descrizione AS settore, dbo_Anag_Finalita_Confer.Descrizione AS finalita, dbo.Anag_Finalita.Descrizione AS finprova, dbo.Conferimenti.Data_Prelievo AS dtprel, 
                         dbo.Conferimenti.Data AS dtconf, dbo.Conferimenti.Data_Accettazione AS dtreg, dbo.Conferimenti.Data_Inserimento AS dtprog, dbo.Esami_Aggregati.Data_Invio AS dtinvio, dbo.Esami_Aggregati.Data_Carico AS dtcarico, 
                         dbo.Esami_Aggregati.Data_Inizio_Analisi AS dtinizio, dbo.Esami_Aggregati.Data_Fine_Analisi AS dtfine, dbo.Esami_Aggregati.Codice AS nprogram, dbo.Anag_Gruppo_Prove.Descrizione AS categprove, 
                         dbo_Anag_Prove_Gruppi.Descrizione AS gruppo, dbo.Anag_Prove.Descrizione AS prova, dbo.Anag_Metodi_di_Prova.Descrizione AS mp, dbo_Anag_Reparti_ConfAcc.Descrizione AS repacc, 
                         dbo_Anag_Reparti_ConfProp.Descrizione AS repprop, dbo.Anag_Reparti.Descrizione AS repanalisi, dbo.Anag_Laboratori.Descrizione AS labanalisi, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato AS primoRdp, 
                         dbo.Esami_Aggregati.Tot_Eseguiti AS nesami, dbo.Anag_TipoConf.Descrizione AS tipopag, dbo.Anag_Tipo_Prel.Descrizione AS tipoprel, dbo.Conferimenti.NrCampioni, dbo.Anag_Comuni.Descrizione AS Comune, 
                         dbo.Anag_Comuni.Provincia, dbo.Anag_Comuni.Regione, dbo.Anag_Comuni.codice_istat, dbo.RDP_Storico.Rdp_Bozza, dbo.RDP_Storico.Data_Stampa, dbo.RDP_Storico.Data_Conferma, dbo.RDP_Storico.Reparto
FROM            dbo.Anag_Registri INNER JOIN
                         dbo.Conferimenti ON dbo.Conferimenti.Registro = dbo.Anag_Registri.Codice LEFT OUTER JOIN
                         dbo.Esami_Aggregati ON dbo.Conferimenti.Anno = dbo.Esami_Aggregati.Anno_Conferimento AND dbo.Conferimenti.Numero = dbo.Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
                         dbo.Nomenclatore_MP ON dbo.Esami_Aggregati.Nomenclatore = dbo.Nomenclatore_MP.Codice LEFT OUTER JOIN
                         dbo.Anag_Metodi_di_Prova ON dbo.Nomenclatore_MP.MP = dbo.Anag_Metodi_di_Prova.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Settori ON dbo.Nomenclatore_MP.Nomenclatore_Settore = dbo.Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore ON dbo.Nomenclatore_Settori.Codice_Nomenclatore = dbo.Nomenclatore.Chiave LEFT OUTER JOIN
                         dbo.Anag_Prove ON dbo.Nomenclatore.Codice_Prova = dbo.Anag_Prove.Codice LEFT OUTER JOIN
                         dbo.Anag_Gruppo_Prove ON dbo.Nomenclatore.Codice_Gruppo = dbo.Anag_Gruppo_Prove.Codice LEFT OUTER JOIN
                         dbo.Programmazione_Finalita ON dbo.Esami_Aggregati.Anno_Conferimento = dbo.Programmazione_Finalita.Anno_Conferimento AND 
                         dbo.Esami_Aggregati.Numero_Conferimento = dbo.Programmazione_Finalita.Numero_Conferimento AND dbo.Esami_Aggregati.Codice = dbo.Programmazione_Finalita.Codice LEFT OUTER JOIN
                         dbo.Anag_Finalita ON dbo.Programmazione_Finalita.Finalita = dbo.Anag_Finalita.Codice LEFT OUTER JOIN
                         dbo.Laboratori_Reparto ON dbo.Esami_Aggregati.RepLab_analisi = dbo.Laboratori_Reparto.Chiave LEFT OUTER JOIN
                         dbo.Anag_Reparti ON dbo.Laboratori_Reparto.Reparto = dbo.Anag_Reparti.Codice LEFT OUTER JOIN
                         dbo.Anag_Laboratori ON dbo.Laboratori_Reparto.Laboratorio = dbo.Anag_Laboratori.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_MP AS dbo_Nomenclatore_MP_Gruppi ON dbo.Esami_Aggregati.Gruppo_Nomenclatore = dbo_Nomenclatore_MP_Gruppi.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore_Settori AS dbo_Nomenclatore_Settori_Gruppi ON dbo_Nomenclatore_MP_Gruppi.Nomenclatore_Settore = dbo_Nomenclatore_Settori_Gruppi.Codice LEFT OUTER JOIN
                         dbo.Nomenclatore AS dbo_Nomenclatore_Gruppi ON dbo_Nomenclatore_Settori_Gruppi.Codice_Nomenclatore = dbo_Nomenclatore_Gruppi.Chiave LEFT OUTER JOIN
                         dbo.Anag_Prove AS dbo_Anag_Prove_Gruppi ON dbo_Nomenclatore_Gruppi.Codice_Prova = dbo_Anag_Prove_Gruppi.Codice INNER JOIN
                         dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfProp ON dbo.Conferimenti.RepLab = dbo_Laboratori_Reparto_ConfProp.Chiave INNER JOIN
                         dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfProp ON dbo_Laboratori_Reparto_ConfProp.Reparto = dbo_Anag_Reparti_ConfProp.Codice INNER JOIN
                         dbo.Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON dbo.Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave INNER JOIN
                         dbo.Anag_Reparti AS dbo_Anag_Reparti_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
                         dbo.Conferimenti_Finalita ON dbo.Conferimenti.Anno = dbo.Conferimenti_Finalita.Anno AND dbo.Conferimenti.Numero = dbo.Conferimenti_Finalita.Numero INNER JOIN
                         dbo.Anag_Finalita AS dbo_Anag_Finalita_Confer ON dbo.Conferimenti_Finalita.Finalita = dbo_Anag_Finalita_Confer.Codice INNER JOIN
                         dbo.Anag_TipoConf ON dbo.Conferimenti.Tipo = dbo.Anag_TipoConf.Codice AND dbo.Esami_Aggregati.Tipo_Pagamento = dbo.Anag_TipoConf.Codice INNER JOIN
                         dbo.Anag_Tipo_Prel ON dbo.Conferimenti.Tipo_Prelievo = dbo.Anag_Tipo_Prel.Codice INNER JOIN
                         dbo.Anag_Comuni ON dbo.Anag_Registri.Codice = dbo.Anag_Comuni.Codice INNER JOIN
                         dbo.RDP_Storico ON dbo.Conferimenti.Anno = dbo.RDP_Storico.Anno
WHERE        (dbo.Laboratori_Reparto.Laboratorio > 1) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND (dbo.Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(dbo.Conferimenti.Data_Accettazione) } = 2022) AND 
                         (dbo.Anag_Registri.Descrizione IN ('Alimenti Uomo', 'Alimenti Zootecnici', 'Sanità Animale'))"







































# queryTaut <- "SELECT
#   dbo.Conferimenti.Data_Prelievo,
#   dbo.Conferimenti.Data,
#   dbo_Anag_Reparti_ConfAcc.Descrizione As stracc,
#   dbo_Anag_Reparti_ConfProp.Descrizione As strapp,
#   dbo.Conferimenti.Numero As nconf,
#   Datename(weekday, dbo.Conferimenti.Data_Accettazione) As giorno,
#   dbo.Conferimenti.Data_Accettazione,
#   dbo.Esami_Aggregati.Codice As codprogram,
#   dbo.Anag_Prove.Descrizione As prova,
#   dbo.Esami_Aggregati.Data_Invio,
#   dbo.Esami_Aggregati.Data_Carico,
#   dbo.Anag_Reparti.Descrizione As repprova,
#   dbo.Anag_Laboratori.Descrizione As Laboratorio,
#   dbo.Esami_Aggregati.Data_Inizio_Analisi,
#   dbo.Esami_Aggregati.Data_Fine_Analisi,
#   dbo.RDP_Date_Emissione.Data_RDP,
#   dbo.Conferimenti.DataOra_Primo_RDP_Completo_Firmato,
#   dbo.Conferimenti.NrCampioni,
#   dbo.Esami_Aggregati.Tot_Eseguiti,
#   dbo.Nomenclatore.Chiave,
#   dbo.Anag_Metodi_di_Prova_Base.Descrizione As mmpp,
#   dbo.Anag_Gruppo_Prove.Descrizione As catprova,
#   dbo.Anag_Registri.Descrizione As reg,
#   dbo.Anag_Tipo_Prel.Descrizione As tipoprelievo,
#   dbo.Anag_Sottotipo_Prel.Descrizione As sottotipoprel,
#   dbo.Anag_Sottotipo_Prel_Dett.Descrizione As stipopreldescr,
#   dbo.Anag_Comuni.Descrizione As Comune,
#   dbo.Anag_Comuni.Provincia As Provincia,
#   dbo.Anag_Regioni.Descrizione As Regione
# FROM
# { oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
#    INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
#    INNER JOIN dbo.Anag_Comuni ON ( dbo.Anag_Comuni.Codice=dbo.Conferimenti.Luogo_Prelievo )
#    LEFT OUTER JOIN dbo.Anag_Regioni ON ( dbo.Anag_Regioni.Codice=dbo.Anag_Comuni.Regione )
#    LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#    LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
#    LEFT OUTER JOIN dbo.Anag_Gruppo_Prove ON ( dbo.Nomenclatore.Codice_Gruppo=dbo.Anag_Gruppo_Prove.Codice )
#    LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova_Revisioni ON ( dbo.Nomenclatore_MP.MP=dbo.Anag_Metodi_di_Prova_Revisioni.Codice )
#    LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova_Base ON ( dbo.Anag_Metodi_di_Prova_Revisioni.MP_Base=dbo.Anag_Metodi_di_Prova_Base.Codice )
#    LEFT OUTER JOIN dbo.Laboratori_Reparto ON ( dbo.Esami_Aggregati.RepLab_analisi=dbo.Laboratori_Reparto.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Reparti ON ( dbo.Laboratori_Reparto.Reparto=dbo.Anag_Reparti.Codice )
#    LEFT OUTER JOIN dbo.Anag_Laboratori ON ( dbo.Laboratori_Reparto.Laboratorio=dbo.Anag_Laboratori.Codice )
#    INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
#    INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
#    INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
#    INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
#    LEFT OUTER JOIN dbo.Anag_Sottotipo_Prel ON ( dbo.Anag_Sottotipo_Prel.Codice=dbo.Conferimenti.SottoTipo_Prelievo )
#    LEFT OUTER JOIN dbo.Anag_Sottotipo_Prel_Dett ON ( dbo.Conferimenti.SottoTipoPrel_Dett=dbo.Anag_Sottotipo_Prel_Dett.Codice )
#    LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
#   }
# WHERE
# ( dbo.Laboratori_Reparto.Laboratorio > 1  )
#   AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  (
#   {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2022
#   AND  dbo.Anag_Registri.Descrizione  NOT IN  ('Altri Controlli (cosmetici,ambientali..)', 'Controlli Interni Sistema Qualità')
#   )
# "
# 
# 
# 
# 
# 
# 
# 
# 
# queryFin <- "SELECT
#    dbo.Conferimenti.Data_Accettazione,
#   dbo.Conferimenti.Numero,
#   dbo_Anag_Finalita_Confer.Descrizione
# FROM
#   dbo.Conferimenti,
#   dbo.Anag_Finalita  dbo_Anag_Finalita_Confer,
#   dbo.Anag_Registri,
#   dbo.Conferimenti_Finalita
# WHERE
#   ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice  )
#   AND  ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero  )
#   AND  ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice  )
#   AND  (
# {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2022
#   AND  dbo.Anag_Registri.Descrizione  NOT IN  ('Altri Controlli (cosmetici,ambientali..)', 'Controlli Interni Sistema Qualità')
#   )
# "
