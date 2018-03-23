000001140909     P*--------------------------------------------------
000002140910     P*--------------------------------------------------
000003140910     P* Procedure name: PropostaBlocco_
000004140910     P* Purpose:
000005140910     P* Returns:        Esito.
000006140910     P*--------------------------------------------------
000007140910     D Set_Stato_Credito...
000008141112     D                 PR            10I 0
000009140910     D  pipartitaiva                 20
000010140910     D  picodiceficale...
000011140910     D                               20
000012141112     D  piidcliente                   8
000013140910     D  piAzione                     10     CONST
000014140910     D  piPGM                        10     CONST
000015140910     P* Procedure name: PropostaBlocco_
000016140910     P* Purpose:        Inizializza modulo.
000017140909     P* Returns:        Esito.
000018140909     P*--------------------------------------------------
000019140909     D Set_Stato_Credito_init...
000020140909     D                 PR            10I 0
000021140909     D  piKpjba                            LIKEDS(kpjba)
000022140909     D                                     OPTIONS(*NOPASS)
000023140909     P*--------------------------------------------------
000024140909     P* Procedure name: PropostaBlocco_
000025140909     P* Purpose:
000026140909     P* Returns:        Esito.
000027140909     P*--------------------------------------------------
000028140911     D Set_Stato_Credito_Esegui...
000029140909     D                 PR            10I 0
000030140911     D  pikcc                         4  0
000031140911     D  piKSC                         7  0
000032140911     D  piAzione                     10    const
000033140911     D  piPGM                        10    CONST
000034140911     P*--------------------------------------------------
000035140909     P* Procedure name: PropostaBlocco_
000036140909     P* Purpose:
000037140909     P* Returns:        Esito.
000038140909     P*--------------------------------------------------
000039140911     D Set_Stato_Credito_Blocco_pagamenti...
000040140911     D                 PR            10I 0
000041140911     D  pikcc                         4  0
000042140911     D  piKSC                         7  0
000043140911     D  piAzione                     10    const
000044140911     P*--------------------------------------------------
000045140909     P* Procedure name: PropostaBlocco_
000046140909     P* Purpose:
000047140909     P* Returns:        Esito.
000048140909     P*--------------------------------------------------
000049140911     D Set_Stato_Credito_Update_CNCLP...
000050140911     D                 PR            10I 0
000051140911     D  pikcc                         4  0
000052140911     D  piksc                         7  0
000053140912     D  piAzione                     10    const
000054140911     D  pistato                       3    const
000074140909     P*--------------------------------------------------
000075140909     P* Procedure name: PropostaBlocco_
000076140909     P* Purpose:
000077140909     P* Returns:        Esito.
000078140909     P*--------------------------------------------------
000081140912     D Set_Stato_Credito_Update_CNACO...
000082140909     D                 PR            10I 0
000083140909     D  pikcc                         4  0
000084140909     D  piksc                         7  0
000085140912     D  piAzione                     10    const
000086140911     P*--------------------------------------------------
000087140911     P* Procedure name: PropostaBlocco_
000088140911     P* Purpose:
000089140911     P* Returns:        Esito.
000090140911     P*--------------------------------------------------
000091140911     D Set_Stato_Credito_Get_Cliente...
000092140911     D                 PR            10I 0
000093140911     D  pikcc                         4  0
000094140911     D  piKSC                         7  0
000095140911     P*--------------------------------------------------
000096140911     P* Procedure name: PropostaBlocco_
000097140911     P* Purpose:
000098140911     P* Returns:        Esito.
000099140911     P*--------------------------------------------------
000100140911     D Set_Stato_Credito_Get_Tabella_BLS...
000101140911     D                 PR            10I 0
000102140911     D  pistato                       3    const
000103140911     P*--------------------------------------------------
000112140911     P* Procedure name: PropostaBlocco_
000113140911     P* Purpose:
000114140911     P* Returns:        Esito.
000115140911     P*--------------------------------------------------
000116140911     D Set_Stato_Credito_Save...
000117140911     D                 PR            10I 0
000118140911     D  piafterbefore                 1    const
000119140911     P*--------------------------------------------------
000120140909     P* Procedure name: PropostaBlocco_
000121140909     P* Purpose:        Recupero i dati della tabella 4W
000122140909     P* Returns:        Esito.
000123140909     P*--------------------------------------------------
000124140909     D Set_Stato_Credito_Get_Tabella_4W...
000125140909     D                 PR            10I 0
000126140909     D  pistato                       3    const
000127140909     D
