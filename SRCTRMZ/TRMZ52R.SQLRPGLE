000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011206     F*          STATISTICA CONSUMI                        *
000300040302     F* SCRIVO da pcagip a agip                            *
000301040302     f* per compilare esistono gli oggetti pcagip_N e ddspcagipn che devono
000302040302     f* essere ridenominati in pcagip e ddspcagip
000303040302     fddspcagip ip   e             disk
005200020607     C*---------------------------------------------------------------*
005201021211     c                   if        WHFLDI = 'ALIQU00001'
005202021211     C* A SECONDA CHE IL CAMPO DELL'IVA SIA NUMERICO O ALFA (POTREBBE
005203021213     C* CONTENERE IL CODICE DI ESENZIONE) MI COMPIA TUTTO IL CONTENUTO
005204040302     C* DEL FILE PCAGIP ECCETTO IL CAMPO FLAG00001 CHE DEVE RIMANERE VUOTO
005205030113     C* VERRA' IMPOSTATO = A QUANDO HO RICEVUTO CORRETTAMENTE O MENO
005206030113     C* IL RECORD (ES. se non ho l'anagrafico contabilizzo lo stesso)
005207021211     c                   IF        WHFLDD <> 0
005300020730     C/EXEC SQL
005301030117     C+ INSERT INTO AGIP (CLIENTE, NUMER00001, DATA_00001,
005302021213     C+ NUMER00002, DATA_00002, SEGNO, COD_CARTA, NOMINATIVO, CENTR00001,
005303021213     C+ COD_A00001, PLAFOND_GG, PLAFOND_MM, PLAFOND_PV, NUMER00003,
005304021213     C+ DATA_00003, ORA_V00001, COD_PV, INDIR00001, LOCAL00001, PROV_PV,
005305021213     C+ COD_P00001, UN_MI00001, DESCR00001, QUANTITA, PREZZO, IMPORTO,
005306021213     C+ SCONTO_LT, SCONTO, FAI_DA_TE, ALIQU00001, IMPOR00001, IMPONIBILE,
005307021213     C+ CHILOMETRI, VALUTA, SOMMIN, FESTIVO, INFOR00001) SELECT CLIENTE,
005308021213     C+ NUMER00001, DATA_00001, NUMER00002, DATA_00002, SEGNO, COD_CARTA,
005309021213     C+ NOMINATIVO, CENTR00001, COD_A00001, PLAFOND_GG, PLAFOND_MM,
005310021213     C+ PLAFOND_PV, NUMER00003, DATA_00003, ORA_V00001, COD_PV, INDIR00001,
005311021213     C+ LOCAL00001, PROV_PV, COD_P00001, UN_MI00001, DESCR00001,
005312021213     C+ QUANTITA, PREZZO, IMPORTO, SCONTO_LT/10000 , SCONTO, FAI_DA_TE,
005313021213     C+ digits(ALIQU00001), IMPOR00001, IMPONIBILE, CHILOMETRI,VALUTA,
005314040302     C+ SOMMIN, FESTIVO, INFOR00001 FROM PCAGIP
005700020730     C/END-EXEC
005701021211     C*
005702021211     C                   IF        sqlcod < 0
005703021211     C                   call      'X66CHGJOB'
005704030113     c                   seton                                        h1
005705021211     C                   end
005706021211     C                   ELSE
005707021211     C                   CALL      'TRMZ52R1'
005713021211     C                   END
005714021211     C                   END
