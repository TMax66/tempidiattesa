queryTaut <- "SELECT
dbo.Conferimenti.Data_Prelievo,
dbo.Conferimenti.Data,
dbo_Anag_Reparti_ConfAcc.Descrizione As stracc,
dbo_Anag_Reparti_ConfProp.Descrizione As strprop,
dbo.Conferimenti.Numero,
dbo_Anag_Finalita_Confer.Descrizione,
Datename(weekday, dbo.Conferimenti.Data_Accettazione) As giorno,
dbo.Conferimenti.Data_Accettazione,
dbo.Esami_Aggregati.Codice,
dbo.Anag_Prove.Descrizione As Prova,
dbo.Esami_Aggregati.Data_Invio,
dbo.Esami_Aggregati.Data_Carico,
dbo.Anag_Reparti.Descrizione As Repprova,
dbo.Anag_Laboratori.Descrizione As Lab,
dbo.Esami_Aggregati.Data_Inizio_Analisi,
dbo.Esami_Aggregati.Data_Fine_Analisi,
dbo.RDP_Date_Emissione.Data_RDP,
dbo.Conferimenti.DataOra_Primo_RDP_Completo_Firmato,
dbo.Conferimenti.NrCampioni,
dbo.Esami_Aggregati.Tot_Eseguiti,
dbo.Nomenclatore.Chiave,
dbo.Anag_Metodi_di_Prova_Base.Descrizione As MP
FROM
{ oj dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
  INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
  LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
  LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
  LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
  LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
  LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
  LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova_Revisioni ON ( dbo.Nomenclatore_MP.MP=dbo.Anag_Metodi_di_Prova_Revisioni.Codice )
  LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova_Base ON ( dbo.Anag_Metodi_di_Prova_Revisioni.MP_Base=dbo.Anag_Metodi_di_Prova_Base.Codice )
  LEFT OUTER JOIN dbo.Laboratori_Reparto ON ( dbo.Esami_Aggregati.RepLab_analisi=dbo.Laboratori_Reparto.Chiave )
  LEFT OUTER JOIN dbo.Anag_Reparti ON ( dbo.Laboratori_Reparto.Reparto=dbo.Anag_Reparti.Codice )
  LEFT OUTER JOIN dbo.Anag_Laboratori ON ( dbo.Laboratori_Reparto.Laboratorio=dbo.Anag_Laboratori.Codice )
  INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
  INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
  INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
  INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
  INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
  LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
}
WHERE
( dbo.Laboratori_Reparto.Laboratorio > 1  )
AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
AND  (
  {fn year(dbo.Conferimenti.Data)}  =  2021
  AND  dbo.Anag_Registri.Descrizione  NOT IN  ('Altri Controlli (cosmetici,ambientali..)', 'Controlli Interni Sistema Qualità')
)"
