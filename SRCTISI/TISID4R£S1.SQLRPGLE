000100170620     H*--------------------------------------------------------------------------------------------*
000200170622     H* DPD GeoRDB 2017 - Calcolo instradamento
000300170620     H*--------------------------------------------------------------------------------------------*
000400170620     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500170620     H DECEDIT('0,') DATEDIT(*DMY.)
000600170620     F*------------------
000700170620     F* DATA BASE
000800170620     F*------------------
000900170626     FTABEL00F  IF   E           K DISK
001000170622     FAZORG01L  IF   E           K DISK
001100170622     FDPCCC14I  IF   E           K DISK
001200170620
001300170620     D*------------------
001400170620     D* DS ESTERNE
001500170620     D*------------------
001600170626     D DPCVE10F      e ds
001700170626     D DPCSO10F      e ds
001800170626     D DPCDP10F      e ds
001900170627     D DPCBU10F      e ds
002000170626     D DPCIC10F      e ds
002100170626     D DPCNT10F      e ds
002200170626     D DPCR210F      e ds
002300170626     D DPCR310F      e ds
002400170626     D DPCR410F      e ds
002500170626     D DPCR510F      e ds
002600170627     D DPCWS10F      e ds
002700170620     D TISID2DS      e ds
002800170622     D TISID4DS      e ds
002900170622     D OG143         e ds
003000170626     D DS3IDP        e ds
003100170620
003200170620     D*------------------
003300170620     D* VARIABILI D WRK
003400170620     D*------------------
003500170626     D stringaSQL      s           2048A   varying inz
003600170626     D api             s              1A   inz('''')
003700170626     D*
003800170626     D sDAT_RIF        s                   like(ISID4DRI)    inz
003900170626     D sVERSION        s                   like(OSID4VER)    inz
004000170626     D*
004100170627     D oVER_DPD        s                   like(CVEVERD)     inz
004200170627     D oVER_BCID       s                   like(CVEBCID)     inz
004300170627     D oSRV_TXT        s                   like(CSOSTXT)     inz
004400170627     D oSRV_MRK        s                   like(CSOSMRK)     inz
004500170627     D oALLOWID        s                   like(CWSALWID)    inz
004600170626     D*
004700170627     D oO_DPT          s                   like(CDPDPTN)     inz
004800170627     D oO_ISO2         s                   like(CDPISO2)     inz
004900170627     D oO_BUCN         s                   like(CDPBUCN)     inz
005000170627     D oO_GRPL         s                   like(CDPGRPID)    inz
005100170628     D oO_ICC          s                   like(CICICC)      inz
005200170626     D*
005300170627     D oD_ISO2         s                   like(CDPISO2)     inz
005400170627     D oD_BUCN         s                   like(CDPBUCN)     inz
005500170627     D oD_BUCA         s                   like(CBUBUCA)     inz
005600170627     D oD_NTWC         s                   like(CNTNTWC)     inz
005700170628     D oD_ISON         s                   like(CCCISON)     inz
005800170626     D*
005900170627     D oD_DPT          s                   like(CR2DDPT)     inz
006000170627     D oD_DPTISO2      s                   like(CDPISO2)     inz
006100170627     D oD_DSTR         s                   like(CDPDSTR)     inz
006200170627     D oD_SSORT        s                   like(CR2SSORT)    inz
006300170627     D oD_PTNC         s                   like(CR2PTNC)     inz
006400170627     D oD_DPTIATA      s                   like(CDPIATA)     inz
006500170626     D*
006600170627     D oD_OSORT        s                   like(CR3OSORT)    inz
006700170627     D oD_CSORT        s                   like(CR4CSORT)    inz
006800170627     D oD_DSORT        s                   like(CR5DSORT)    inz
006900170626     D*
007000170626     D gBU             s                   like(§3IBUCN) inz
007100170627     D gISO2           s                   like(§3IISO2) inz
007200170622     D SkLNP           s              3S 0 dim(500) inz
007300170622     D SkDPT           s              7A   dim(500) inz
007400170622     D jLNP            s              3S 0 inz
007500170622     D sjLNP           s                   like(jLNP) inz
007600170622     D SkNZD           s              3A   dim(500) inz
007700170622     D SkISO2          s              2A   dim(500) inz
007800170622     D jNZD            s              3S 0 inz
007900170622     D sjNZD           s                   like(jNZD) inz
008000170622     D wFound          s              1A   inz
008100170628     D wNotFoundCR1    s              1A   inz
008200170628     D wNotFoundCR2    s              1A   inz
008300170628     D wNotFoundCR3    s              1A   inz
008400170628     D wNotFoundCR4    s              1A   inz
008500170628     D wNotFoundCR5    s              1A   inz
008600170627     D wAllow          s              1A   inz
008700170626     D*
008800170627     D wBUFFER         s            256A   inz
008900170629     D DS_CSO          ds                  qualified inz
009000170627     D   oSRV_TXT                          like(oSRV_TXT)
009100170627     D   oSRV_MRK                          like(oSRV_MRK)
009200170626     D*
009300170627     D DS_OCDP         ds                  qualified inz
009400170627     D   oO_ISO2                           like(oO_ISO2)
009500170627     D   oO_BUCN                           like(oO_BUCN)
009600170627     D   oO_GRPL                           like(oO_GRPL)
009700170626     D*
009800170627     D DS_DCDP         ds                  qualified inz
009900170627     D   oD_DPTISO2                        like(oD_DPTISO2)
010000170627     D   oD_DSTR                           like(oD_DSTR)
010100170627     D   oD_DPTIATA                        like(oD_DPTIATA)
010200170620
010300170620
010400170620     C*--------------------------------------------------------------------------------------------*
010500170620     C* Main lines
010600170620     C*--------------------------------------------------------------------------------------------*
010700150518     C*
010800150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
010900150518     C
011000150518     C/EXEC SQL
011100150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
011200150518     C/END-EXEC
011300170620     C*
011400170620     C* Avvio il monitoring del intero flusso
011500170620     C                   monitor
011600170622     C*
011700170622     C* Resetto indicatore d errore
011800170622     C                   setoff                                       70
011900170620     C*
012000170620     C* Inizializzazioni
012100170622     C                   exsr      exeINZ
012200170622     C*
012300170622     C* Step 01 - Verifica parametri di input
012400170626     C  N70              exsr      chkPAR
012500170622     C*
012600170622     C* Step 02 - Reperimento dati Versione
012700170622     C  N70              exsr      rtvVER
012800170622     C*
012900170626     C* Step 03 - Reperimento Depot DPD (partenza)
013000170626     C  N70              exsr      rtvDPT
013100170622     C*
013200170626     C* Step 04 - Reperimento DESTINATION Nazione ISO 2
013300170626     C  N70              exsr      rtvDISO2
013400170622     C*
013500170622     C* Step 05 - Verifica del Pattern CAP
013600170622     C  N70              exsr      chkPATT
013700170622     C*
013800170622     C* Step 06 - Verifica esistenza CAP in DPD
013900170622     C  N70              exsr      chkPTC
014000170622     C*
014100170622     C* Step 07 - Reperimento dati SOCODE
014200170622     C  N70              exsr      rtvSORC
014300170622     C*
014400170622     C* Step 08 - Reperimento dati ORIGIN DEPOT
014500170626     C  N70              exsr      rtvODPT
014600170626     C*
014700170626     C* Step 09 - Reperimento dati INJECTION CODE
014800170626     C  N70              exsr      rtvCIC
014900170626     C*
015000170626     C* Step 10 - Esecuzione ricerca R1
015100170626     C  N70              exsr      rtvCR1
015200170626     C*
015300170626     C* Step 11 - Esecuzione ricerca R2
015400170626     C  N70              exsr      rtvCR2
015500170626     C*
015600170627     C* Step 12 - Verifica instradamenti consentiti ALLOW
015700170627     C  N70              exsr      chkALW
015800170626     C*
015900170626     C* Step 13 - Esecuzione ricerca R3
016000170626     C  N70              exsr      rtvCR3
016100170626     C*
016200170626     C* Step 14 - Esecuzione ricerca R4
016300170626     C  N70              exsr      rtvCR4
016400170626     C*
016500170626     C* Step 15 - Esecuzione ricerca R5
016600170626     C  N70              exsr      rtvCR5
016700170620     C*
016800170627     C* Step 16 - Valorizzo paramteri di output
016900170627     C  N70              exsr      exeOUT
017000170620     C*
017100170620     C* Gestisco eventuale errore
017200170620     C                   on-error
017300170620     C                   dump(A)
017400170627     C                   eval      OSID4ERR = 'E'
017500170620     C*
017600170620     C* Arresto il monitoring
017700170620     C                   endmon
017800170620     C*
017900170622     C                   seton                                        rt
018000060515     C*
018100170627
018200170627
018300170622     C*------------------------------------------------------------------------*
018400170622     C* EXEINZ - Inizializzazioni
018500170622     C*------------------------------------------------------------------------*
018600170627     C     EXEINZ        BEGSR
018700170622     C*
018800170627     C* Variabili di work
018900170627     C                   clear                   oD_BUCN
019000170627     C                   clear                   oD_BUCA
019100170627     C                   clear                   oD_NTWC
019200170628     C                   clear                   oD_ISON
019300170627     C                   clear                   oD_DPTISO2
019400170627     C                   clear                   oD_DPT
019500170627     C                   clear                   oD_DSTR
019600170627     C                   clear                   oD_SSORT
019700170627     C                   clear                   oD_PTNC
019800170627     C                   clear                   oD_OSORT
019900170627     C                   clear                   oD_CSORT
020000170627     C                   clear                   oD_DSORT
020100170627     C                   clear                   oVER_BCID
020200170627     C                   clear                   oSRV_TXT
020300170627     C                   clear                   oSRV_MRK
020400170627     C                   clear                   oO_DPT
020500170627     C                   clear                   oO_ISO2
020600170627     C                   clear                   oO_BUCN
020700170627     C                   clear                   oO_GRPL
020800170627     C                   clear                   oO_ICC
020900170627     C                   clear                   oALLOWID
021000170627     C                   clear                   oD_DPTIATA
021100170627     C*
021200170627     C* Paramteri di output
021300170629     C                   clear                   USID4CAD
021400170627     C                   clear                   OSID4VER
021500170627     C                   clear                   OSID4VERD
021600170627     C                   clear                   OSID4VDDE
021700170627     C                   clear                   OSID4DBUCN
021800170627     C                   clear                   OSID4DBUCA
021900170627     C                   clear                   OSID4DNTWC
022000170627     C                   clear                   OSID4DDPTC
022100170627     C                   clear                   OSID4DDPT
022200170627     C                   clear                   OSID4DSTR
022300170627     C                   clear                   OSID4SSORT
022400170627     C                   clear                   OSID4DPTNC
022500170627     C                   clear                   OSID4OSORT
022600170627     C                   clear                   OSID4CSORT
022700170627     C                   clear                   OSID4DSORT
022800170627     C                   clear                   OSID4BCID
022900170627     C                   clear                   OSID4SRVX
023000170627     C                   clear                   OSID4SRVM
023100170627     C                   clear                   OSID4ODPT
023200170627     C                   clear                   OSID4ODPTC
023300170627     C                   clear                   OSID4OBUCN
023400170627     C                   clear                   OSID4OGRPL
023500170627     C                   clear                   OSID4OICC
023600170627     C                   clear                   OSID4ALWID
023700170629     C                   clear                   OSID4IATA
023800170629     C                   clear                   OSID4CADP
023900170627     C                   clear                   OSID4ERRL
024000170627     C                   clear                   OSID4ERRD
024100170622     C*
024200170627     C* Inizializzo l'esito
024300170627     C                   movel     '0'           OSID4ERR
024400170627     C*
024500170622     C                   ENDSR
024600170622
024700170622
024800170622     C*------------------------------------------------------------------------*
024900170622     C* CHKPAR - Verifica parametri di input
025000170622     C*------------------------------------------------------------------------*
025100170622     C     CHKPAR        BEGSR
025200170622     C*
025300170622     C                   setoff                                       40
025400170622     C*
025500170622     C* Data di riferimento
025600170622     C                   if        ISID4DRI = *zeros
025700170626     C                   eval      ISID4DRI = datcor
025800170622     C                   endif
025900170622     C*
026000170622     C* Linea di Partenza
026100170622     C                   if        ISID4LNP = *zeros
026200170622     C                   seton                                        40
026300170622     C                   endif
026400170622     C*
026500170622     C* CAP destinatario
026600170622     C                   if        ISID4CAD = *blanks
026700170622     C                   seton                                        40
026800170629     C                   else
026900170629     C                   eval      USID4CAD = ISID4CAD
027000170622     C                   endif
027100170629     C*
027200170629     C* Forzatura su CAP destinatario per NAZIONE IRLANDA
027300170629     C                   if        ISID4NZD = 'IRL'
027400170629     C                   eval      USID4CAD = '1'
027500170629     C                   endif
027600170622     C*
027700170622     C* Servizio DPD
027800170622     C                   if        ISID4SORC = *zeros
027900170622     C                   seton                                        40
028000170622     C                   endif
028100170622     C*
028200170622     C* Se nn ok => imposto subito l'errore generale d procedura
028300170622     C                   if        *in40
028400170622     C                   eval      OSID4ERR  = '1'
028500170622     C                   eval      OSID4ERRL = '92'
028600170622     C                   eval      OSID4ERRD = 'Errore: parametri obbligatori '+
028700170622     C                             'non valorizzati'
028800170622     C                   seton                                        70
028900170622     C                   endif
029000170622     C*
029100170622     C                   ENDSR
029200170622
029300170622
029400170622     C*------------------------------------------------------------------------*
029500170622     C* RTVVER - Reperimento VERSIONE richiesta
029600170622     C*------------------------------------------------------------------------*
029700170622     C     RTVVER        BEGSR
029800170622     C*
029900170622     C* Reperisco VERSIONE solo se non ancora reperita o data riferimento cambiata
030000170626     C                   if        sVERSION  = *zeros    OR
030100170626     C                             ISID4DRI <> sDAT_RIF
030200170626     C*
030300170626     C                   eval      sDAT_RIF = ISID4DRI
030400170622     C*
030500170622     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
030600170627     C                   call      'TISID1R'
030700170626     C                   parm                    sDAT_RIF
030800170626     C                   parm                    sVERSION
030900170627     C                   parm                    oVER_DPD
031000170627     C                   parm                    CVEDDE
031100170627     C                   parm                    CVEDSC
031200170626     C                   parm                    oVER_BCID
031300170622     C*
031400170622     C* Se reperita versione valida
031500170626     C                   if        sVERSION  > *zeros
031600170622     C                   else
031700170622     C*
031800170622     C* Se non ok => imposto subito l'errore generale d procedura
031900170622     C                   eval      OSID4ERR  = '1'
032000170622     C                   eval      OSID4ERRL = '90'
032100170622     C                   eval      OSID4ERRD = 'Errore in reperimento versione'+
032200170622     C                             ' (DPCVE10F)'
032300170622     C                   seton                                        70
032400170622     C                   endif
032500170622     C                   endif
032600170622     C*
032700170622     C                   ENDSR
032800170622
032900170622
033000170622     C*------------------------------------------------------------------------*
033100170626     C* RTVDPT - Reperimento DEPOT DPD (partenza)
033200170622     C*------------------------------------------------------------------------*
033300170626     C     RTVDPT        BEGSR
033400170622     C*
033500170626     C                   clear                   oO_DPT
033600170622     C*
033700170622     C* Cerco abbinamento Filiale BRT <=> Depot DPD
033800170627     C                   z-add     1             jLNP
033900170622     C     ISID4LNP      lookup    SkLNP(jLNP)                            55
034000170622     C                   if        %found
034100170626     C                   eval      oO_DPT = SkDPT(jLNP)
034200170622     C                   else
034300170622     C*
034400170622     C     ISID4LNP      chain     azorg01l
034500170622     C                   if        %found(azorg01l)
034600170622     C                   eval      OG143 = ORGDE3
034700170622     C                   add       1             sjLNP
034800170626     C                   eval      SkLNP(sjLNP) = ISID4LNP
034900170627     C                   eval      SkDPT(sjLNP) = %editc(gBU:'X')+§OGDP1
035000170626     C                   eval      oO_DPT = SkDPT(sjLNP)
035100170622     C                   endif
035200170622     C*
035300170626     C                   if        oO_DPT = *blanks
035400170622     C                   eval      OSID4ERR  = '1'
035500170622     C                   eval      OSID4ERRL = '91'
035600170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
035700170622     C                             'partenza DPD (AZORG01L)'
035800170622     C                   seton                                        70
035900170622     C                   endif
036000170622     C                   endif
036100170622     C*
036200170622     C                   ENDSR
036300170622
036400170622
036500170622     C*------------------------------------------------------------------------*
036600170626     C* RTVDISO2 - Reperimento DESTINATION CODICE NAZIONE ISO 2
036700170622     C*------------------------------------------------------------------------*
036800170626     C     RTVDISO2      BEGSR
036900170622     C*
037000170622     C                   clear                   oD_ISO2
037100170622     C*
037200170622     C* Cerco abbinamento Cod Nazione BRT <=> Codice Nazione ISO2
037300170627     C                   z-add     1             jNZD
037400170622     C     ISID4NZD      lookup    SkNZD(jNZD)                            55
037500170622     C                   if        %found
037600170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
037700170622     C                   else
037800170622     C*
037900170622     C                   eval      cccATB = *blanks
038000170626     C                   eval      cccVER = sVERSION
038100170622     C                   eval      cccBRT = ISID4NZD
038200170622     C     KEYccc14_C    chain     dpccc14i
038300170622     C                   if        %found(dpccc14i)
038400170622     C                   add       1             sjNZD
038500170622     C                   eval      SkNZD(sjNZD)  = ISID4NZD
038600170622     C                   eval      SkISO2(sjNZD) = cccISO2
038700170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
038800170622     C                   endif
038900170622     C*
039000170622     C                   if        oD_ISO2 = *blanks
039100170622     C                   eval      OSID4ERR  = '1'
039200170622     C                   eval      OSID4ERRL = '93'
039300170622     C                   eval      OSID4ERRD = 'Errore in reperimento nazione '+
039400170622     C                             'destino (DPCCC10F)'
039500170622     C                   seton                                        70
039600170622     C                   endif
039700170622     C                   endif
039800170622     C*
039900170622     C                   ENDSR
040000170620
040100170620
040200170620     C*------------------------------------------------------------------------*
040300170620     C* CHKPATT - Verifica del Pattern CAP
040400170620     C*------------------------------------------------------------------------*
040500170620     C     CHKPATT       BEGSR
040600170622     C*
040700170622     C* Inizializzazioni
040800170622     C                   clear                   TISID2DS
040900170622     C*
041000170622     C* Valorizzazioni
041100170627     C                   eval      ISID2VER   = sVERSION
041200170627     C                   eval      ISID2NISO2 = oD_ISO2
041300170629     C                   eval      ISID2PTC   = USID4CAD
041400170622     C                   call      'TISID2R'
041500170622     C                   parm                    TISID2DS
041600170622     C*
041700170622     C                   if        OSID2ESI  = 'V'
041800170622     C                   eval      USID4CAD = OSID2PTC
041900170622     C                   else
042000170622     C                   eval      OSID4ERR  = '1'
042100170627     C                   eval      OSID4ERRL = '99'
042200170627     C                   eval      OSID4ERRD = 'Errore - CAP errato ' +
042300170622     C                             '(Pattern DPCCC10F)'
042400170622     C                   seton                                        70
042500170622     C                   endif
042600170620     C*
042700170620     C                   ENDSR
042800170622
042900170622
043000170622     C*------------------------------------------------------------------------*
043100170622     C* CHKPTC - Verifica esistenza CAP in DPD
043200170622     C*------------------------------------------------------------------------*
043300170622     C     CHKPTC        BEGSR
043400170622     C*
043500170622     C* Inizializzazioni
043600170622     C                   movel     *blanks       wFound
043700170629     C*
043800170629     C* Se necessaria validazione del CAP rispetto alla NAZIONE
043900170629     C                   if        OSID2PVAL = 'VB'                             * blocked on failure
044000170629     C
044100170622     C/EXEC SQL
044200170622     C+ SET :wFound = (SELECT '1' FROM DPCPC10F WHERE CPCATB = ' ' AND
044300170626     C+                CPCVER = :sVERSION AND CPCISO2 = :oD_ISO2
044400170626     C+                AND :USID4CAD BETWEEN CPCPTCB AND CPCPTCE
044500170626     C+                FETCH FIRST 1 ROW ONLY)
044600170622     C/END-EXEC
044700170622     C
044800170622     C*
044900170622     C                   if        wFound = *on
045000170622     C                   else
045100170622     C                   eval      OSID4ERR  = '1'
045200170627     C                   eval      OSID4ERRL = '99'
045300170627     C                   eval      OSID4ERRD = 'Errore - CAP non trovato ' +
045400170622     C                             '(DPCPC10F)'
045500170622     C                   seton                                        70
045600170622     C                   endif
045700170629     C*
045800170629     C                   endif
045900170622     C*
046000170622     C                   ENDSR
046100170622
046200170622
046300170622     C*------------------------------------------------------------------------*
046400170622     C* RTVSORC - Reperimento dati SOCODE
046500170622     C*------------------------------------------------------------------------*
046600170622     C     RTVSORC       BEGSR
046700170622     C*
046800170622     C* Inizializzazioni
046900170627     C                   clear                   DS_CSO
047000170627     C                   clear                   wBUFFER
047100170622     C
047200170622     C/EXEC SQL
047300170629     C+ SET :wBUFFER = (SELECT '1' CONCAT CSOSTXT CONCAT CSOSMRK FROM DPCSO10F
047400170627     C+              WHERE CSOATB = ' ' AND CSOVER = :sVERSION AND
047500170627     C+              CSOSORC = :ISID4SORC
047600170627     C+              FETCH FIRST 1 ROW ONLY)
047700170622     C/END-EXEC
047800170622     C
047900170622     C*
048000170629     C                   if        %subst(wBUFFER:1:1) = *on
048100170628     C                   eval      DS_CSO   = %subst(wBUFFER:2)
048200170627     C                   eval      oSRV_TXT = DS_CSO.oSRV_TXT
048300170627     C                   eval      oSRV_MRK = DS_CSO.oSRV_MRK
048400170622     C                   else
048500170622     C                   eval      OSID4ERR  = '1'
048600170627     C                   eval      OSID4ERRL = '95'
048700170627     C                   eval      OSID4ERRD = 'Errore in reperimento servizio'+
048800170622     C                             ' (DPCSC10F)'
048900170622     C                   seton                                        70
049000170622     C                   endif
049100170622     C*
049200170622     C                   ENDSR
049300170622
049400170622
049500170622     C*------------------------------------------------------------------------*
049600170626     C* RTVODPT - Reperimento dati ORIGIN DEPOT
049700170622     C*------------------------------------------------------------------------*
049800170626     C     RTVODPT       BEGSR
049900170622     C*
050000170622     C* Inizializzazioni
050100170627     C                   clear                   DS_OCDP
050200170627     C                   clear                   wBUFFER
050300170622     C
050400170622     C/EXEC SQL
050500170627     C+ SET :wBUFFER  = (SELECT
050600170627     C+               '1' CONCAT CDPISO2 CONCAT digits(CDPBUCN) CONCAT CDPGRPID
050700170627     C+               FROM DPCDP10F
050800170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
050900170627     C+               CDPDPTN = :oO_DPT
051000170627     C+               FETCH FIRST 1 ROW ONLY)
051100170622     C/END-EXEC
051200170622     C
051300170622     C*
051400170627     C                   if        %subst(wBUFFER:1:1) = *on
051500170628     C                   eval      DS_OCDP = %subst(wBUFFER:2)
051600170627     C                   eval      oO_ISO2 = DS_OCDP.oO_ISO2
051700170627     C                   eval      oO_BUCN = DS_OCDP.oO_BUCN
051800170627     C                   eval      oO_GRPL = DS_OCDP.oO_GRPL
051900170622     C                   else
052000170622     C                   eval      OSID4ERR  = '1'
052100170626     C                   eval      OSID4ERRL = '91'
052200170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
052300170622     C                             'partenza DPD (DPCDP10F)'
052400170622     C                   seton                                        70
052500170622     C                   endif
052600170622     C*
052700170622     C                   ENDSR
052800170626
052900170626
053000170626     C*------------------------------------------------------------------------*
053100170626     C* RTVCIC - Reperimento dati INJECTION CODE
053200170626     C*------------------------------------------------------------------------*
053300170626     C     RTVCIC        BEGSR
053400170626     C*
053500170626     C* Inizializzazioni
053600170627     C                   clear                   wBUFFER
053700170626     C*
053800170626     C* Se indicato uns specifico SUBCODE in input
053900170626     C                   if        ISID4SUBC <> *blanks
054000170627     C                   eval      oO_ICC = oO_DPT + ISID4SUBC
054100170626     C
054200170626     C/EXEC SQL
054300170627     C+ SET :wBUFFER  = (SELECT '1' CONCAT CICICC
054400170627     C+               FROM DPCIC10F
054500170627     C+               WHERE CICATB = ' ' AND CICVER = :sVERSION AND
054600170627     C+               CICICC = :oO_ICC
054700170627     C+               FETCH FIRST 1 ROW ONLY)
054800170626     C/END-EXEC
054900170626     C
055000170626     C*
055100170627     C                   if        %subst(wBUFFER:1:1) = *on
055200170626     C                   else
055300170626     C                   eval      OSID4ERR  = '1'
055400170626     C                   eval      OSID4ERRL = '94'
055500170626     C                   eval      OSID4ERRD = 'Errore in reperimento ' +
055600170626     C                             'INJECTION CODE (DPCIC10F)'
055700170626     C                   seton                                        70
055800170626     C                   endif
055900170626     C*
056000170626     C                   endif
056100170626     C*
056200170626     C                   ENDSR
056300170626
056400170626
056500170626     C*------------------------------------------------------------------------*
056600170626     C* RTVCR1 - Esecuzione ricerca R1
056700170626     C*------------------------------------------------------------------------*
056800170626     C     RTVCR1        BEGSR
056900170626     C*
057000170626     C* Inizializzazioni
057100170626     C                   clear                   oD_BUCN
057200170626     C                   clear                   oD_BUCA
057300170628     C                   clear                   oD_NTWC
057400170628     C                   clear                   oD_ISON
057500170626     C                   clear                   stringaSQL
057600170628     C                   clear                   wNotFoundCR1
057700170626     C*
057800170626     C* Compongo la stringa SQL
057900170626       stringaSQL =
058000170626       'SELECT CR1BUCN FROM DPCR110F ' +
058100170626       'WHERE CR1ATB=§ § and CR1VER='+%editc(sVERSION:'4')+' and ' +
058200170627       'CR1TRADS=§'+ISID4TRADS+'§ and ' +
058300170626       'CR1ISO2=§'+%trim(oD_ISO2)+'§ and (CR1PTCB = § § or ' +
058400170626       '§'+%trim(USID4CAD)+'§ between CR1PTCB and CR1PTCE) and ' +
058500170626       '(CR1NTWP=§ § or CR1NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE §' +
058600170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
058700170626       ' (CR1ROUT=§ § and CR1ROUPB=§ §) or' +
058800170627       ' (CR1ROUT=§I§ and §'+oO_ICC+'§ between CR1ROUPB and CR1ROUPE) or'+
058900170627       ' (CR1ROUT=§C§ and CR1ROUPB=§'+oO_ISO2+'§) or ' +
059000170627       '(CR1ROUT=§D§ and §'+oO_DPT+'§ between CR1ROUPB and CR1ROUPE) or'+
059100170626       ' (CR1ROUT=§G§ and CR1ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
059200170627       ' (CR1ROUT=§B§ and CR1ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
059300170626       ') ' +
059400170626       'ORDER BY CR1PTY DESC ' +
059500170627       'FETCH FIRST 1 ROW ONLY ';
059600170627
059700170626     C*
059800170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
059900170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
060000170626     C
060100170626     C* Quindi eseguo stringa SQL così ottenuta
060200170626     C*
060300170626     C/EXEC SQL
060400170626     C+ prepare S_R1 from :stringaSQL
060500170626     C/END-EXEC
060600170626     C
060700170626     C/EXEC SQL
060800170626     C+ declare C_R1 asensitive cursor for S_R1
060900170626     C/END-EXEC
061000170626     C
061100170626     C/EXEC SQL
061200170626     C+ open C_R1
061300170626     C/END-EXEC
061400170626     C
061500170626     C/EXEC SQL
061600170626     C+ Fetch C_R1 into :oD_BUCN
061700170626     C/END-EXEC
061800170626     C*
061900170626     C                   if        sqlcod = *zeros
062000170626     C*
062100170626     C* Quindi aggancio i dati della DESTINATION BUSINESS UNIT
062200170626     C
062300170626     C/EXEC SQL
062400170626     C+ SET :oD_BUCA = (SELECT CBUBUCA FROM DPCBU10F
062500170626     C+                 WHERE CBUATB = ' ' AND CBUVER = :sVERSION AND
062600170627     C+                 CBUBUCN = :oD_BUCN
062700170627     C+                 FETCH FIRST 1 ROW ONLY)
062800170626     C/END-EXEC
062900170626     C                   if        sqlcod <> *zeros
063000170628     C                   eval      wNotFoundCR1 = '1'
063100170626     C                   endif
063200170626     C*
063300170626     C* Quindi aggancio i dati del DESTINATION NETWORK CODE
063400170626     C
063500170626     C/EXEC SQL
063600170626     C+ SET :oD_NTWC = (SELECT CNTNTWC FROM DPCNT10F
063700170626     C+                 WHERE CNTATB = ' ' AND CNTVER = :sVERSION AND
063800170626     C+                 CNTISO2 = :oD_ISO2 AND CNTBUCN = :oD_BUCN
063900170626     C+                 FETCH FIRST 1 ROW ONLY)
064000170626     C/END-EXEC
064100170628     C                   if        sqlcod = *zeros
064200170628     C                   else
064300170628     C*
064400170628     C* Quindi aggancio i dati della NAZIONE
064500170628     C
064600170628     C/EXEC SQL
064700170628     C+ SET :oD_ISON = (SELECT CCCISON FROM DPCCC10F
064800170628     C+                 WHERE CCCATB = ' ' AND CCCVER = :sVERSION AND
064900170628     C+                 CCCISO2 = :oD_ISO2
065000170628     C+                 FETCH FIRST 1 ROW ONLY)
065100170628     C/END-EXEC
065200170628     C                   if        sqlcod = *zeros
065300170628     C                   eval      oD_NTWC = %editc(oD_ISON:'X')
065400170628     C                   else
065500170628     C                   eval      wNotFoundCR1 = '1'
065600170626     C                   endif
065700170628     C                   endif
065800170626     C*
065900170626     C                   else
066000170628     C                   eval      wNotFoundCR1 = '1'
066100170626     C                   endif
066200170626     C
066300170626     C/EXEC SQL
066400170626     C+ close C_R1
066500170626     C/END-EXEC
066600170626     C
066700170626     C*
066800170628     C***                if        wNotFoundCR1 = *on
066900170627     C***                eval      OSID4ERR  = '1'
067000170627     C***                eval      OSID4ERRL = '96'
067100170627     C***                eval      OSID4ERRD = 'Errore in calcolo ' +
067200170627     C***                          'instradamento R1 (DPCR110F)'
067300170627     C***                seton                                        70
067400170627     C***                endif
067500170626     C*
067600170626     C                   ENDSR
067700170626
067800170626
067900170626     C*------------------------------------------------------------------------*
068000170626     C* RTVCR2 - Esecuzione ricerca R2
068100170626     C*------------------------------------------------------------------------*
068200170626     C     RTVCR2        BEGSR
068300170626     C*
068400170626     C* Inizializzazioni
068500170626     C                   clear                   oD_DPT
068600170626     C                   clear                   oD_DPTISO2
068700170626     C                   clear                   oD_DSTR
068800170626     C                   clear                   oD_SSORT
068900170626     C                   clear                   oD_PTNC
069000170627     C                   clear                   oD_DPTIATA
069100170626     C                   clear                   stringaSQL
069200170628     C                   clear                   wNotFoundCR2
069300170627     C                   clear                   DS_DCDP
069400170627     C                   clear                   wBUFFER
069500170626     C*
069600170626     C* Compongo la stringa SQL
069700170626       stringaSQL =
069800170626       'SELECT CR2DDPT, CR2SSORT, CR2PTNC FROM DPCR210F ' +
069900170626       'WHERE CR2ATB=§ § and CR2VER='+%editc(sVERSION:'4')+' and ' +
070000170626       'CR2TRADS=§'+%trim(ISID4TRADS)+'§ and ' +
070100170626       'CR2BUCN='+%editc(oD_BUCN:'X')+' and ' +
070200170626       'CR2ISO2=§'+%trim(oD_ISO2)+'§ and (CR2PTCB = § § or ' +
070300170626       '§'+%trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE) and ' +
070400170626       '(CR2NTWP=§ § or CR2NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE §' +
070500170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
070600170626       ' (CR2ROUT=§ § and CR2ROUPB=§ §) or' +
070700170627       ' (CR2ROUT=§I§ and §'+oO_ICC+'§ between CR2ROUPB and CR2ROUPE) or'+
070800170627       ' (CR2ROUT=§C§ and CR2ROUPB=§'+oO_ISO2+'§) or ' +
070900170627       '(CR2ROUT=§D§ and §'+oO_DPT+'§ between CR2ROUPB and CR2ROUPE) or'+
071000170626       ' (CR2ROUT=§G§ and CR2ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
071100170627       ' (CR2ROUT=§B§ and CR2ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
071200170626       ') ' +
071300170626       'ORDER BY CR2PTY DESC ' +
071400170627       'FETCH FIRST 1 ROW ONLY ';
071500170627
071600170626     C*
071700170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
071800170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
071900170626     C
072000170626     C* Quindi eseguo stringa SQL così ottenuta
072100170626     C*
072200170626     C/EXEC SQL
072300170626     C+ prepare S_R2 from :stringaSQL
072400170626     C/END-EXEC
072500170626     C
072600170626     C/EXEC SQL
072700170626     C+ declare C_R2 asensitive cursor for S_R2
072800170626     C/END-EXEC
072900170626     C
073000170626     C/EXEC SQL
073100170626     C+ open C_R2
073200170626     C/END-EXEC
073300170626     C
073400170626     C/EXEC SQL
073500170626     C+ Fetch C_R2 into :oD_DPT, :oD_SSORT, :oD_PTNC
073600170626     C/END-EXEC
073700170626     C*
073800170626     C                   if        sqlcod = *zeros
073900170626     C*
074000170626     C* Quindi aggancio i dati del DESTINATION DEPOT
074100170626     C
074200170627     C/EXEC SQL
074300170627     C+ SET :wBUFFER  = (SELECT
074400170627     C+               '1' CONCAT CDPISO2 CONCAT CDPDSTR CONCAT CDPIATA
074500170627     C+               FROM DPCDP10F
074600170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
074700170627     C+               CDPDPTN = :oD_DPT
074800170627     C+               FETCH FIRST 1 ROW ONLY)
074900170626     C/END-EXEC
075000170626     C
075100170626     C*
075200170627     C                   if        %subst(wBUFFER:1:1) = *on
075300170628     C                   eval      DS_DCDP    = %subst(wBUFFER:2)
075400170627     C                   eval      oD_DPTISO2 = DS_DCDP.oD_DPTISO2
075500170627     C                   eval      oD_DSTR    = DS_DCDP.oD_DSTR
075600170627     C                   eval      oD_DPTIATA = DS_DCDP.oD_DPTIATA
075700170626     C                   else
075800170626     C                   eval      oD_DPTISO2 = oD_ISO2
075900170626     C                   endif
076000170626     C*
076100170626     C                   else
076200170628     C                   eval      wNotFoundCR2 = '1'
076300170626     C                   endif
076400170626     C
076500170626     C/EXEC SQL
076600170626     C+ close C_R2
076700170626     C/END-EXEC
076800170626     C
076900170626     C*
077000170628     C                   if        wNotFoundCR2 = *on
077100170626     C                   eval      OSID4ERR  = '1'
077200170626     C                   eval      OSID4ERRL = '96'
077300170626     C                   eval      OSID4ERRD = 'Errore in calcolo ' +
077400170626     C                             'instradamento R2 (DPCR210F)'
077500170626     C                   seton                                        70
077600170626     C                   endif
077700170626     C*
077800170626     C                   ENDSR
077900170626
078000170626
078100170626     C*------------------------------------------------------------------------*
078200170626     C* RTVCR3 - Esecuzione ricerca R3
078300170626     C*------------------------------------------------------------------------*
078400170626     C     RTVCR3        BEGSR
078500170626     C*
078600170626     C* Inizializzazioni
078700170626     C                   clear                   oD_OSORT
078800170626     C                   clear                   stringaSQL
078900170628     C                   clear                   wNotFoundCR3
079000170626     C*
079100170626     C* Compongo la stringa SQL
079200170626       stringaSQL =
079300170626       'SELECT CR3OSORT FROM DPCR310F ' +
079400170626       'WHERE CR3ATB=§ § and CR3VER='+%editc(sVERSION:'4')+' and ' +
079500170626       'CR3ISO2=§'+%trim(oD_ISO2)+'§ and ' +
079600170626       '(CR3DDPT=§ § or CR3DDPT=§'+oD_DPT+'§) and ' +
079700170626       '(CR3PTCB=§ § or ' +
079800170626       '§'+%trim(USID4CAD)+'§ between CR3PTCB and CR3PTCE) and ' +
079900170626       '(CR3NTWP=§ § or CR3NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE §' +
080000170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
080100170626       ' (CR3ROUT=§ § and CR3ROUPB=§ §) or' +
080200170627       ' (CR3ROUT=§I§ and §'+oO_ICC+'§ between CR3ROUPB and CR3ROUPE) or'+
080300170627       ' (CR3ROUT=§C§ and CR3ROUPB=§'+oO_ISO2+'§) or ' +
080400170627       '(CR3ROUT=§D§ and §'+oO_DPT+'§ between CR3ROUPB and CR3ROUPE) or'+
080500170626       ' (CR3ROUT=§G§ and CR3ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
080600170627       ' (CR3ROUT=§B§ and CR3ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
080700170626       ') ' +
080800170626       'ORDER BY CR3PTY DESC ' +
080900170627       'FETCH FIRST 1 ROW ONLY ';
081000170627
081100170626     C*
081200170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
081300170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
081400170626     C
081500170626     C* Quindi eseguo stringa SQL così ottenuta
081600170626     C*
081700170626     C/EXEC SQL
081800170626     C+ prepare S_R3 from :stringaSQL
081900170626     C/END-EXEC
082000170626     C
082100170626     C/EXEC SQL
082200170626     C+ declare C_R3 asensitive cursor for S_R3
082300170626     C/END-EXEC
082400170626     C
082500170626     C/EXEC SQL
082600170626     C+ open C_R3
082700170626     C/END-EXEC
082800170626     C
082900170626     C/EXEC SQL
083000170626     C+ Fetch C_R3 into :oD_OSORT
083100170626     C/END-EXEC
083200170626     C*
083300170626     C                   if        sqlcod = *zeros
083400170626     C                   else
083500170628     C                   eval      wNotFoundCR3 = '1'
083600170626     C                   endif
083700170626     C
083800170626     C/EXEC SQL
083900170626     C+ close C_R3
084000170626     C/END-EXEC
084100170627     C
084200170626     C*
084300170626     C                   ENDSR
084400170627
084500170627
084600170627     C*------------------------------------------------------------------------*
084700170627     C* CHKALW - Verifica instradamenti consentiti ALLOW
084800170627     C*------------------------------------------------------------------------*
084900170627     C     CHKALW        BEGSR
085000170627     C*
085100170627     C* Inizializzazioni
085200170627     C                   clear                   oALLOWID
085300170627     C                   clear                   stringaSQL
085400170627     C                   clear                   wAllow
085500170627     C*
085600170627     C* Compongo la stringa SQL
085700170627       stringaSQL =
085800170627       'SELECT CWSALWID FROM DPCWS10F ' +
085900170627       'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
086000170627       '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
086100170627       '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
086200170627       '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
086300170627       '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
086400170627       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and §' +
086500170627       %editc(ISID4SORC:'X')+'§ between CWSRULSB and CWSRULSE and (' +
086600170627       '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
086700170627       '(CWSRULTT=§C§ and CWSRULTB=§'+oD_DPTISO2+'§) or ' +
086800170627       '(CWSRULTT=§Z§ and §'+oD_DPTISO2+%trim(USID4CAD)+'§ ' +
086900170627                                      'between CWSRULTB and CWSRULTE)) ' +
087000170627       'FETCH FIRST 1 ROW ONLY ';
087100170627
087200170627     C*
087300170627     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
087400170627     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
087500170627     C
087600170627     C* Quindi eseguo stringa SQL così ottenuta
087700170627     C*
087800170627     C/EXEC SQL
087900170627     C+ prepare S_WS from :stringaSQL
088000170627     C/END-EXEC
088100170627     C
088200170627     C/EXEC SQL
088300170627     C+ declare C_WS asensitive cursor for S_WS
088400170627     C/END-EXEC
088500170627     C
088600170627     C/EXEC SQL
088700170627     C+ open C_WS
088800170627     C/END-EXEC
088900170627     C
089000170627     C/EXEC SQL
089100170627     C+ Fetch C_WS into :oALLOWID
089200170627     C/END-EXEC
089300170627     C*
089400170627     C                   if        sqlcod = *zeros
089500170627     C                   eval      wAllow = '1'
089600170627     C                   endif
089700170627     C
089800170627     C/EXEC SQL
089900170627     C+ close C_WS
090000170627     C/END-EXEC
090100170627     C
090200170627     C                   if        wAllow = *on
090300170627     C                   else
090400170627     C                   eval      OSID4ERR  = '1'
090500170627     C                   eval      OSID4ERRL = '97'
090600170627     C                   eval      OSID4ERRD = 'Instradamento non ' +
090700170627     C                             'consentito. (DPCWS10F)'
090800170627     C                   seton                                        70
090900170627     C                   endif
091000170627     C*
091100170627     C                   ENDSR
091200170626
091300170626
091400170626     C*------------------------------------------------------------------------*
091500170626     C* RTVCR4 - Esecuzione ricerca R4
091600170626     C*------------------------------------------------------------------------*
091700170626     C     RTVCR4        BEGSR
091800170626     C*
091900170626     C* Inizializzazioni
092000170626     C                   clear                   oD_CSORT
092100170626     C                   clear                   stringaSQL
092200170628     C                   clear                   wNotFoundCR4
092300170626     C*
092400170626     C* Compongo la stringa SQL
092500170626       stringaSQL =
092600170627       'SELECT CR4CSORT FROM DPCR410F ' +
092700170626       'WHERE CR4ATB=§ § and CR4VER='+%editc(sVERSION:'4')+' and ' +
092800170626       'CR4ISO2=§'+%trim(oD_ISO2)+'§ and ' +
092900170626       '(CR4DDPT=§ § or CR4DDPT=§'+oD_DPT+'§) and ' +
093000170626       '(CR4NTWP=§ § or CR4NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE §' +
093100170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
093200170626       ' (CR4ROUT=§ § and CR4ROUPB=§ §) or' +
093300170627       ' (CR4ROUT=§I§ and §'+oO_ICC+'§ between CR4ROUPB and CR4ROUPE) or'+
093400170627       ' (CR4ROUT=§C§ and CR4ROUPB=§'+oO_ISO2+'§) or ' +
093500170627       '(CR4ROUT=§D§ and §'+oO_DPT+'§ between CR4ROUPB and CR4ROUPE) or'+
093600170626       ' (CR4ROUT=§G§ and CR4ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
093700170627       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
093800170626       ') ' +
093900170626       'ORDER BY CR4PTY DESC ' +
094000170627       'FETCH FIRST 1 ROW ONLY ';
094100170627
094200170626     C*
094300170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
094400170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
094500170626     C
094600170626     C* Quindi eseguo stringa SQL così ottenuta
094700170626     C*
094800170626     C/EXEC SQL
094900170626     C+ prepare S_R4 from :stringaSQL
095000170626     C/END-EXEC
095100170626     C
095200170626     C/EXEC SQL
095300170626     C+ declare C_R4 asensitive cursor for S_R4
095400170626     C/END-EXEC
095500170626     C
095600170626     C/EXEC SQL
095700170626     C+ open C_R4
095800170626     C/END-EXEC
095900170626     C
096000170626     C/EXEC SQL
096100170626     C+ Fetch C_R4 into :oD_CSORT
096200170626     C/END-EXEC
096300170626     C*
096400170626     C                   if        sqlcod = *zeros
096500170626     C                   else
096600170628     C                   eval      wNotFoundCR4 = '1'
096700170626     C                   endif
096800170626     C
096900170626     C/EXEC SQL
097000170626     C+ close C_R4
097100170626     C/END-EXEC
097200170626     C
097300170626     C*
097400170626     C                   ENDSR
097500170626
097600170626
097700170626     C*------------------------------------------------------------------------*
097800170626     C* RTVCR5 - Esecuzione ricerca R5
097900170626     C*------------------------------------------------------------------------*
098000170626     C     RTVCR5        BEGSR
098100170626     C*
098200170626     C* Inizializzazioni
098300170626     C                   clear                   oD_DSORT
098400170626     C                   clear                   stringaSQL
098500170628     C                   clear                   wNotFoundCR5
098600170626     C*
098700170626     C* Compongo la stringa SQL
098800170626       stringaSQL =
098900170627       'SELECT CR5DSORT FROM DPCR510F ' +
099000170626       'WHERE CR5ATB=§ § and CR5VER='+%editc(sVERSION:'4')+' and ' +
099100170627       'CR5DDPT=§'+oD_DPT+'§ and ' +
099200170626       '(CR5PTCB=§ § or ' +
099300170626       '§'+%trim(USID4CAD)+'§ between CR5PTCB and CR5PTCE) and ' +
099400170626       '(CR5NTWP=§ § or CR5NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE §' +
099500170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
099600170626       ' (CR5ROUT=§ § and CR5ROUPB=§ §) or' +
099700170627       ' (CR5ROUT=§I§ and §'+oO_ICC+'§ between CR5ROUPB and CR5ROUPE) or'+
099800170627       ' (CR5ROUT=§C§ and CR5ROUPB=§'+oO_ISO2+'§) or ' +
099900170627       '(CR5ROUT=§D§ and §'+oO_DPT+'§ between CR5ROUPB and CR5ROUPE) or'+
100000170626       ' (CR5ROUT=§G§ and CR5ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
100100170627       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
100200170626       ') ' +
100300170626       'ORDER BY CR5PTY DESC ' +
100400170627       'FETCH FIRST 1 ROW ONLY ';
100500170627
100600170626     C*
100700170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
100800170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
100900170626     C
101000170626     C* Quindi eseguo stringa SQL così ottenuta
101100170626     C*
101200170626     C/EXEC SQL
101300170626     C+ prepare S_R5 from :stringaSQL
101400170626     C/END-EXEC
101500170626     C
101600170626     C/EXEC SQL
101700170626     C+ declare C_R5 asensitive cursor for S_R5
101800170626     C/END-EXEC
101900170626     C
102000170626     C/EXEC SQL
102100170626     C+ open C_R5
102200170626     C/END-EXEC
102300170626     C
102400170626     C/EXEC SQL
102500170626     C+ Fetch C_R5 into :oD_DSORT
102600170626     C/END-EXEC
102700170626     C*
102800170626     C                   if        sqlcod = *zeros
102900170626     C                   else
103000170628     C                   eval      wNotFoundCR5 = '1'
103100170626     C                   endif
103200170626     C
103300170626     C/EXEC SQL
103400170626     C+ close C_R5
103500170626     C/END-EXEC
103600170626     C
103700170626     C*
103800170626     C                   ENDSR
103900170627
104000170627
104100170627     C*------------------------------------------------------------------------*
104200170627     C* EXEOUT - Valorizzo paramteri di output
104300170627     C*------------------------------------------------------------------------*
104400170627     C     EXEOUT        BEGSR
104500170627     C*
104600170627     C                   eval      OSID4VER   = sVERSION
104700170627     C                   eval      OSID4VERD  = oVER_DPD
104800170627     C                   eval      OSID4VDDE  = CVEDDE
104900170627     C                   eval      OSID4DBUCN = oD_BUCN
105000170627     C                   eval      OSID4DBUCA = oD_BUCA
105100170627     C                   eval      OSID4DNTWC = oD_NTWC
105200170627     C                   eval      OSID4DDPTC = oD_DPTISO2
105300170627     C                   eval      OSID4DDPT  = oD_DPT
105400170627     C                   eval      OSID4DSTR  = oD_DSTR
105500170627     C                   eval      OSID4SSORT = oD_SSORT
105600170627     C                   eval      OSID4DPTNC = oD_PTNC
105700170627     C                   eval      OSID4OSORT = oD_OSORT
105800170627     C                   eval      OSID4CSORT = oD_CSORT
105900170627     C                   eval      OSID4DSORT = oD_DSORT
106000170627     C                   eval      OSID4BCID  = oVER_BCID
106100170627     C                   eval      OSID4SRVX  = oSRV_TXT
106200170627     C                   eval      OSID4SRVM  = oSRV_MRK
106300170627     C                   eval      OSID4ODPT  = oO_DPT
106400170627     C                   eval      OSID4ODPTC = oO_ISO2
106500170627     C                   eval      OSID4OBUCN = oO_BUCN
106600170627     C                   eval      OSID4OGRPL = oO_GRPL
106700170627     C                   eval      OSID4OICC  = oO_ICC
106800170627     C                   eval      OSID4ALWID = oALLOWID
106900170627     C                   eval      OSID4IATA  = oD_DPTIATA
107000170629     C                   evalr     OSID4CADP  = %trim(USID4CAD)
107100170629     C                   eval      OSID4CADP = %scanrpl(' ':'0':OSID4CADP)
107200170627     C*
107300170627     C* Sancisco esito a OK
107400170627     C                   movel     *blanks       OSID4ERR
107500170627     C*
107600170627     C                   ENDSR
107700170626
107800170626
107900170626     C*------------------------------------------------------------------------*
108000170626     C* CARTAB - Caricamemto tabelle si procedura
108100170626     C*------------------------------------------------------------------------*
108200170626     C     CARTAB        BEGSR
108300170626     C*
108400170626     C* Reperisco i valori tabellati di procedura
108500170626     C                   eval      tblKUT = 1
108600170626     C                   eval      tblCOD = '3I'
108700170626     C                   eval      tblKEY ='DPD'
108800170626     C     KEYtab00_C    chain     tabel00f
108900170626     C                   if        %found(tabel00f)
109000170626     C                   eval      DS3IDP = tblUNI
109100170627     C                   eval      gBU   = §3IBUCN
109200170627     C                   eval      gISO2 = §3IISO2
109300170626     C                   else
109400170626     C                   eval      OSID4ERR  = '1'
109500170627     C                   eval      OSID4ERRL = '80'
109600170627     C                   eval      OSID4ERRD = 'Tabella 3I valore §3IBUC non ' +
109700170626     C                             'trovato'
109800170626     C                   seton                                        70
109900170626     C                   endif
110000170626     C*
110100170626     C                   ENDSR
110200170620
110300170620
110400170620     C*--------------------------------------------------------------------------------------------*
110500060515     C* *inzsr - operazioni iniziali
110600060515     C*--------------------------------------------------------------------------------------------*
110700000000     C     *inzsr        BEGSR
110800060515     C*
110900060515     C* Ricevimento parametri
111000060515     C     *ENTRY        PLIST
111100170627     C                   PARM                    TISID4DS
111200060510     C*
111300060510     C* CALCOLA LA DATA CORRENTE
111400170620     C                   Z-ADD     *zeros        datcor            8 0
111500170620     C                   EVAL      datcor = %dec(%date() : *iso)
111600170626     C*
111700170626     C* CARICAMENTO TABELLE DI PROCEDURA
111800170626     C                   EXSR      CARTAB
111900170626     C*
112000170626     C* TABEL00F - Completa
112100170626     C     KEYtab00_C    KLIST
112200170626     C                   KFLD                    tblKUT
112300170626     C                   KFLD                    tblCOD
112400170626     C                   KFLD                    tblKEY
112500170622     C*
112600170622     C* DPCCC14I - Completa
112700170622     C     KEYccc14_C    KLIST
112800170622     C                   KFLD                    cccATB
112900170622     C                   KFLD                    cccVER
113000170622     C                   KFLD                    cccBRT
113100060515     C*
113200060515     C                   ENDSR
