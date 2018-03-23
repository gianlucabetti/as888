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
005800170629     D oD_GRPL         s                   like(CDPGRPID)    inz
005900170626     D*
006000170627     D oD_DPT          s                   like(CR2DDPT)     inz
006100170627     D oD_DPTISO2      s                   like(CDPISO2)     inz
006200170627     D oD_DSTR         s                   like(CDPDSTR)     inz
006300170627     D oD_SSORT        s                   like(CR2SSORT)    inz
006400170627     D oD_PTNC         s                   like(CR2PTNC)     inz
006500170627     D oD_DPTIATA      s                   like(CDPIATA)     inz
006600170626     D*
006700170627     D oD_OSORT        s                   like(CR3OSORT)    inz
006800170627     D oD_CSORT        s                   like(CR4CSORT)    inz
006900170627     D oD_DSORT        s                   like(CR5DSORT)    inz
007000170626     D*
007100170626     D gBU             s                   like(§3IBUCN) inz
007200170627     D gISO2           s                   like(§3IISO2) inz
007300170622     D SkLNP           s              3S 0 dim(500) inz
007400170622     D SkDPT           s              7A   dim(500) inz
007500170622     D jLNP            s              3S 0 inz
007600170622     D sjLNP           s                   like(jLNP) inz
007700170913     D SkNZD           s              3A   dim(500) inz(*loval)
007800170622     D SkISO2          s              2A   dim(500) inz
007900170622     D jNZD            s              3S 0 inz
008000170622     D sjNZD           s                   like(jNZD) inz
008100170622     D wFound          s              1A   inz
008200170628     D wNotFoundCR1    s              1A   inz
008300170628     D wNotFoundCR2    s              1A   inz
008400170628     D wNotFoundCR3    s              1A   inz
008500170628     D wNotFoundCR4    s              1A   inz
008600170628     D wNotFoundCR5    s              1A   inz
008700170627     D wAllow          s              1A   inz
008800170626     D*
008900170627     D wBUFFER         s            256A   inz
009000170629     D DS_CSO          ds                  qualified inz
009100170627     D   oSRV_TXT                          like(oSRV_TXT)
009200170627     D   oSRV_MRK                          like(oSRV_MRK)
009300170626     D*
009400170627     D DS_OCDP         ds                  qualified inz
009500170627     D   oO_ISO2                           like(oO_ISO2)
009600170627     D   oO_BUCN                           like(oO_BUCN)
009700170627     D   oO_GRPL                           like(oO_GRPL)
009800170626     D*
009900170627     D DS_DCDP         ds                  qualified inz
010000170627     D   oD_DPTISO2                        like(oD_DPTISO2)
010100170627     D   oD_DSTR                           like(oD_DSTR)
010200170627     D   oD_DPTIATA                        like(oD_DPTIATA)
010300170629     D   oD_GRPL                           like(oD_GRPL)
010400170620
010500170620
010600170620     C*--------------------------------------------------------------------------------------------*
010700170620     C* Main lines
010800170620     C*--------------------------------------------------------------------------------------------*
010900150518     C*
011000150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
011100150518     C
011200150518     C/EXEC SQL
011300150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
011400150518     C/END-EXEC
011500170620     C*
011600170620     C* Avvio il monitoring del intero flusso
011700170620     C                   monitor
011800170622     C*
011900170622     C* Resetto indicatore d errore
012000170622     C                   setoff                                       70
012100170620     C*
012200170620     C* Inizializzazioni
012300170622     C                   exsr      exeINZ
012400170622     C*
012500170622     C* Step 01 - Verifica parametri di input
012600170626     C  N70              exsr      chkPAR
012700170622     C*
012800170622     C* Step 02 - Reperimento dati Versione
012900170622     C  N70              exsr      rtvVER
013000170622     C*
013100170626     C* Step 03 - Reperimento Depot DPD (partenza)
013200170626     C  N70              exsr      rtvDPT
013300170622     C*
013400170626     C* Step 04 - Reperimento DESTINATION Nazione ISO 2
013500170626     C  N70              exsr      rtvDISO2
013600170622     C*
013700170622     C* Step 05 - Verifica del Pattern CAP
013800170622     C  N70              exsr      chkPATT
013900170622     C*
014000170622     C* Step 06 - Verifica esistenza CAP in DPD
014100170622     C  N70              exsr      chkPTC
014200170622     C*
014300170622     C* Step 07 - Reperimento dati SOCODE
014400170622     C  N70              exsr      rtvSORC
014500170622     C*
014600170622     C* Step 08 - Reperimento dati ORIGIN DEPOT
014700170626     C  N70              exsr      rtvODPT
014800170626     C*
014900170626     C* Step 09 - Reperimento dati INJECTION CODE
015000170626     C  N70              exsr      rtvCIC
015100170626     C*
015200170626     C* Step 10 - Esecuzione ricerca R1
015300170626     C  N70              exsr      rtvCR1
015400170626     C*
015500170626     C* Step 11 - Esecuzione ricerca R2
015600170626     C  N70              exsr      rtvCR2
015700170626     C*
015800170627     C* Step 12 - Verifica instradamenti consentiti ALLOW
015900170627     C  N70              exsr      chkALW
016000170626     C*
016100170626     C* Step 13 - Esecuzione ricerca R3
016200170626     C  N70              exsr      rtvCR3
016300170626     C*
016400170626     C* Step 14 - Esecuzione ricerca R4
016500170626     C  N70              exsr      rtvCR4
016600170626     C*
016700170626     C* Step 15 - Esecuzione ricerca R5
016800170626     C  N70              exsr      rtvCR5
016900170620     C*
017000170627     C* Step 16 - Valorizzo paramteri di output
017100170627     C  N70              exsr      exeOUT
017200170620     C*
017300170620     C* Gestisco eventuale errore
017400170620     C                   on-error
017500170620     C                   dump(A)
017600170627     C                   eval      OSID4ERR = 'E'
017700170620     C*
017800170620     C* Arresto il monitoring
017900170620     C                   endmon
018000170620     C*
018100170622     C                   seton                                        rt
018200060515     C*
018300170627
018400170627
018500170622     C*------------------------------------------------------------------------*
018600170622     C* EXEINZ - Inizializzazioni
018700170622     C*------------------------------------------------------------------------*
018800170627     C     EXEINZ        BEGSR
018900170622     C*
019000170627     C* Variabili di work
019100170627     C                   clear                   oD_BUCN
019200170627     C                   clear                   oD_BUCA
019300170627     C                   clear                   oD_NTWC
019400170628     C                   clear                   oD_ISON
019500170627     C                   clear                   oD_DPTISO2
019600170627     C                   clear                   oD_DPT
019700170627     C                   clear                   oD_DSTR
019800170627     C                   clear                   oD_SSORT
019900170627     C                   clear                   oD_PTNC
020000170627     C                   clear                   oD_OSORT
020100170627     C                   clear                   oD_CSORT
020200170627     C                   clear                   oD_DSORT
020300170627     C                   clear                   oSRV_TXT
020400170627     C                   clear                   oSRV_MRK
020500170627     C                   clear                   oO_DPT
020600170627     C                   clear                   oO_ISO2
020700170627     C                   clear                   oO_BUCN
020800170627     C                   clear                   oO_GRPL
020900170627     C                   clear                   oO_ICC
021000170627     C                   clear                   oALLOWID
021100170627     C                   clear                   oD_DPTIATA
021200170627     C*
021300170627     C* Paramteri di output
021400170629     C                   clear                   USID4CAD
021500170627     C                   clear                   OSID4VER
021600170627     C                   clear                   OSID4VERD
021700170627     C                   clear                   OSID4VDDE
021800170627     C                   clear                   OSID4DBUCN
021900170627     C                   clear                   OSID4DBUCA
022000170627     C                   clear                   OSID4DNTWC
022100170627     C                   clear                   OSID4DDPTC
022200170627     C                   clear                   OSID4DDPT
022300170627     C                   clear                   OSID4DSTR
022400170627     C                   clear                   OSID4SSORT
022500170627     C                   clear                   OSID4DPTNC
022600170627     C                   clear                   OSID4OSORT
022700170627     C                   clear                   OSID4CSORT
022800170627     C                   clear                   OSID4DSORT
022900170627     C                   clear                   OSID4BCID
023000170627     C                   clear                   OSID4SRVX
023100170627     C                   clear                   OSID4SRVM
023200170627     C                   clear                   OSID4ODPT
023300170627     C                   clear                   OSID4ODPTC
023400170627     C                   clear                   OSID4OBUCN
023500170627     C                   clear                   OSID4OGRPL
023600170627     C                   clear                   OSID4OICC
023700170627     C                   clear                   OSID4ALWID
023800170629     C                   clear                   OSID4IATA
023900170629     C                   clear                   OSID4CADP
024000170627     C                   clear                   OSID4ERRL
024100170627     C                   clear                   OSID4ERRD
024200170622     C*
024300170627     C* Inizializzo l'esito
024400170627     C                   movel     '0'           OSID4ERR
024500170627     C*
024600170622     C                   ENDSR
024700170622
024800170622
024900170622     C*------------------------------------------------------------------------*
025000170622     C* CHKPAR - Verifica parametri di input
025100170622     C*------------------------------------------------------------------------*
025200170622     C     CHKPAR        BEGSR
025300170622     C*
025400170622     C                   setoff                                       40
025500170622     C*
025600170622     C* Data di riferimento
025700170622     C                   if        ISID4DRI = *zeros
025800170626     C                   eval      ISID4DRI = datcor
025900170622     C                   endif
026000170622     C*
026100170622     C* Linea di Partenza
026200170622     C                   if        ISID4LNP = *zeros
026300170622     C                   seton                                        40
026400170622     C                   endif
026500170622     C*
026600170622     C* CAP destinatario
026700170622     C                   if        ISID4CAD = *blanks
026800170622     C                   seton                                        40
026900170629     C                   else
027000170629     C                   eval      USID4CAD = ISID4CAD
027100170622     C                   endif
027200170629     C*
027300170629     C* Forzatura su CAP destinatario per NAZIONE IRLANDA
027400170629     C                   if        ISID4NZD = 'IRL'
027500170629     C                   eval      USID4CAD = '1'
027600170629     C                   endif
027700170622     C*
027800170622     C* Servizio DPD
027900170622     C                   if        ISID4SORC = *zeros
028000170622     C                   seton                                        40
028100170622     C                   endif
028200170622     C*
028300170622     C* Se nn ok => imposto subito l'errore generale d procedura
028400170622     C                   if        *in40
028500170622     C                   eval      OSID4ERR  = '1'
028600170622     C                   eval      OSID4ERRL = '92'
028700170622     C                   eval      OSID4ERRD = 'Errore: parametri obbligatori '+
028800170622     C                             'non valorizzati'
028900170622     C                   seton                                        70
029000170622     C                   endif
029100170622     C*
029200170622     C                   ENDSR
029300170622
029400170622
029500170622     C*------------------------------------------------------------------------*
029600170622     C* RTVVER - Reperimento VERSIONE richiesta
029700170622     C*------------------------------------------------------------------------*
029800170622     C     RTVVER        BEGSR
029900170622     C*
030000170622     C* Reperisco VERSIONE solo se non ancora reperita o data riferimento cambiata
030100170626     C                   if        sVERSION  = *zeros    OR
030200170626     C                             ISID4DRI <> sDAT_RIF
030300170626     C*
030400170626     C                   eval      sDAT_RIF = ISID4DRI
030500170622     C*
030600170622     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
030700170627     C                   call      'TISID1R'
030800170626     C                   parm                    sDAT_RIF
030900170626     C                   parm                    sVERSION
031000170627     C                   parm                    oVER_DPD
031100170627     C                   parm                    CVEDDE
031200170627     C                   parm                    CVEDSC
031300170626     C                   parm                    oVER_BCID
031400170622     C*
031500170622     C* Se reperita versione valida
031600170626     C                   if        sVERSION  > *zeros
031700170622     C                   else
031800170622     C*
031900170622     C* Se non ok => imposto subito l'errore generale d procedura
032000170622     C                   eval      OSID4ERR  = '1'
032100170622     C                   eval      OSID4ERRL = '90'
032200170622     C                   eval      OSID4ERRD = 'Errore in reperimento versione'+
032300170622     C                             ' (DPCVE10F)'
032400170622     C                   seton                                        70
032500170622     C                   endif
032600170622     C                   endif
032700170622     C*
032800170622     C                   ENDSR
032900170622
033000170622
033100170622     C*------------------------------------------------------------------------*
033200170626     C* RTVDPT - Reperimento DEPOT DPD (partenza)
033300170622     C*------------------------------------------------------------------------*
033400170626     C     RTVDPT        BEGSR
033500170622     C*
033600170626     C                   clear                   oO_DPT
033700170622     C*
033800170622     C* Cerco abbinamento Filiale BRT <=> Depot DPD
033900170627     C                   z-add     1             jLNP
034000170622     C     ISID4LNP      lookup    SkLNP(jLNP)                            55
034100170622     C                   if        %found
034200170626     C                   eval      oO_DPT = SkDPT(jLNP)
034300170622     C                   else
034400170622     C*
034500170622     C     ISID4LNP      chain     azorg01l
034600170622     C                   if        %found(azorg01l)
034700170622     C                   eval      OG143 = ORGDE3
034800170622     C                   add       1             sjLNP
034900170626     C                   eval      SkLNP(sjLNP) = ISID4LNP
035000170627     C                   eval      SkDPT(sjLNP) = %editc(gBU:'X')+§OGDP1
035100170626     C                   eval      oO_DPT = SkDPT(sjLNP)
035200170622     C                   endif
035300170622     C*
035400170626     C                   if        oO_DPT = *blanks
035500170622     C                   eval      OSID4ERR  = '1'
035600170622     C                   eval      OSID4ERRL = '91'
035700170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
035800170622     C                             'partenza DPD (AZORG01L)'
035900170622     C                   seton                                        70
036000170622     C                   endif
036100170622     C                   endif
036200170622     C*
036300170622     C                   ENDSR
036400170622
036500170622
036600170622     C*------------------------------------------------------------------------*
036700170626     C* RTVDISO2 - Reperimento DESTINATION CODICE NAZIONE ISO 2
036800170622     C*------------------------------------------------------------------------*
036900170626     C     RTVDISO2      BEGSR
037000170622     C*
037100170622     C                   clear                   oD_ISO2
037200170622     C*
037300170622     C* Cerco abbinamento Cod Nazione BRT <=> Codice Nazione ISO2
037400170627     C                   z-add     1             jNZD
037500170622     C     ISID4NZD      lookup    SkNZD(jNZD)                            55
037600170913     C                   if        %found
037700170630     C                   eval      oD_ISO2 = SkISO2(jNZD)
037800170622     C                   else
037900170622     C*
038000170622     C                   eval      cccATB = *blanks
038100170626     C                   eval      cccVER = sVERSION
038200170622     C                   eval      cccBRT = ISID4NZD
038300170622     C     KEYccc14_C    chain     dpccc14i
038400170622     C                   if        %found(dpccc14i)
038500170622     C                   add       1             sjNZD
038600170622     C                   eval      SkNZD(sjNZD)  = ISID4NZD
038700170622     C                   eval      SkISO2(sjNZD) = cccISO2
038800170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
038900170622     C                   endif
039000170622     C*
039100170622     C                   if        oD_ISO2 = *blanks
039200170622     C                   eval      OSID4ERR  = '1'
039300170622     C                   eval      OSID4ERRL = '93'
039400170622     C                   eval      OSID4ERRD = 'Errore in reperimento nazione '+
039500170622     C                             'destino (DPCCC10F)'
039600170622     C                   seton                                        70
039700170622     C                   endif
039800170622     C                   endif
039900170622     C*
040000170622     C                   ENDSR
040100170620
040200170620
040300170620     C*------------------------------------------------------------------------*
040400170620     C* CHKPATT - Verifica del Pattern CAP
040500170620     C*------------------------------------------------------------------------*
040600170620     C     CHKPATT       BEGSR
040700170622     C*
040800170622     C* Inizializzazioni
040900170622     C                   clear                   TISID2DS
041000170622     C*
041100170622     C* Valorizzazioni
041200170627     C                   eval      ISID2VER   = sVERSION
041300170627     C                   eval      ISID2NISO2 = oD_ISO2
041400170629     C                   eval      ISID2PTC   = USID4CAD
041500170622     C                   call      'TISID2R'
041600170622     C                   parm                    TISID2DS
041700170622     C*
041800170622     C                   if        OSID2ESI  = 'V'
041900170622     C                   eval      USID4CAD = OSID2PTC
042000170622     C                   else
042100170622     C                   eval      OSID4ERR  = '1'
042200170627     C                   eval      OSID4ERRL = '99'
042300170627     C                   eval      OSID4ERRD = 'Errore - CAP errato ' +
042400170622     C                             '(Pattern DPCCC10F)'
042500170622     C                   seton                                        70
042600170622     C                   endif
042700170620     C*
042800170620     C                   ENDSR
042900170622
043000170622
043100170622     C*------------------------------------------------------------------------*
043200170622     C* CHKPTC - Verifica esistenza CAP in DPD
043300170622     C*------------------------------------------------------------------------*
043400170622     C     CHKPTC        BEGSR
043500170622     C*
043600170622     C* Inizializzazioni
043700170622     C                   movel     *blanks       wFound
043800170629     C*
043900170629     C* Se necessaria validazione del CAP rispetto alla NAZIONE
044000170629     C                   if        OSID2PVAL = 'VB'                             * blocked on failure
044100170629     C
044200170622     C/EXEC SQL
044300170622     C+ SET :wFound = (SELECT '1' FROM DPCPC10F WHERE CPCATB = ' ' AND
044400170626     C+                CPCVER = :sVERSION AND CPCISO2 = :oD_ISO2
044500170626     C+                AND :USID4CAD BETWEEN CPCPTCB AND CPCPTCE
044600170626     C+                FETCH FIRST 1 ROW ONLY)
044700170622     C/END-EXEC
044800170622     C
044900170622     C*
045000170622     C                   if        wFound = *on
045100170622     C                   else
045200170622     C                   eval      OSID4ERR  = '1'
045300170630     C                   eval      OSID4ERRL = '96'
045400170627     C                   eval      OSID4ERRD = 'Errore - CAP non trovato ' +
045500170622     C                             '(DPCPC10F)'
045600170622     C                   seton                                        70
045700170622     C                   endif
045800170629     C*
045900170629     C                   endif
046000170622     C*
046100170622     C                   ENDSR
046200170622
046300170622
046400170622     C*------------------------------------------------------------------------*
046500170622     C* RTVSORC - Reperimento dati SOCODE
046600170622     C*------------------------------------------------------------------------*
046700170622     C     RTVSORC       BEGSR
046800170622     C*
046900170622     C* Inizializzazioni
047000170627     C                   clear                   DS_CSO
047100170627     C                   clear                   wBUFFER
047200170622     C
047300170622     C/EXEC SQL
047400170629     C+ SET :wBUFFER = (SELECT '1' CONCAT CSOSTXT CONCAT CSOSMRK FROM DPCSO10F
047500170627     C+              WHERE CSOATB = ' ' AND CSOVER = :sVERSION AND
047600170627     C+              CSOSORC = :ISID4SORC
047700170627     C+              FETCH FIRST 1 ROW ONLY)
047800170622     C/END-EXEC
047900170622     C
048000170622     C*
048100170629     C                   if        %subst(wBUFFER:1:1) = *on
048200170628     C                   eval      DS_CSO   = %subst(wBUFFER:2)
048300170627     C                   eval      oSRV_TXT = DS_CSO.oSRV_TXT
048400170627     C                   eval      oSRV_MRK = DS_CSO.oSRV_MRK
048500170622     C                   else
048600170622     C                   eval      OSID4ERR  = '1'
048700170627     C                   eval      OSID4ERRL = '95'
048800170627     C                   eval      OSID4ERRD = 'Errore in reperimento servizio'+
048900170622     C                             ' (DPCSC10F)'
049000170622     C                   seton                                        70
049100170622     C                   endif
049200170622     C*
049300170622     C                   ENDSR
049400170622
049500170622
049600170622     C*------------------------------------------------------------------------*
049700170626     C* RTVODPT - Reperimento dati ORIGIN DEPOT
049800170622     C*------------------------------------------------------------------------*
049900170626     C     RTVODPT       BEGSR
050000170622     C*
050100170622     C* Inizializzazioni
050200170627     C                   clear                   DS_OCDP
050300170627     C                   clear                   wBUFFER
050400170622     C
050500170622     C/EXEC SQL
050600170627     C+ SET :wBUFFER  = (SELECT
050700170627     C+               '1' CONCAT CDPISO2 CONCAT digits(CDPBUCN) CONCAT CDPGRPID
050800170627     C+               FROM DPCDP10F
050900170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
051000170627     C+               CDPDPTN = :oO_DPT
051100170627     C+               FETCH FIRST 1 ROW ONLY)
051200170622     C/END-EXEC
051300170622     C
051400170622     C*
051500170627     C                   if        %subst(wBUFFER:1:1) = *on
051600170628     C                   eval      DS_OCDP = %subst(wBUFFER:2)
051700170627     C                   eval      oO_ISO2 = DS_OCDP.oO_ISO2
051800170627     C                   eval      oO_BUCN = DS_OCDP.oO_BUCN
051900170627     C                   eval      oO_GRPL = DS_OCDP.oO_GRPL
052000170622     C                   else
052100170622     C                   eval      OSID4ERR  = '1'
052200170626     C                   eval      OSID4ERRL = '91'
052300170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
052400170622     C                             'partenza DPD (DPCDP10F)'
052500170622     C                   seton                                        70
052600170622     C                   endif
052700170622     C*
052800170622     C                   ENDSR
052900170626
053000170626
053100170626     C*------------------------------------------------------------------------*
053200170626     C* RTVCIC - Reperimento dati INJECTION CODE
053300170626     C*------------------------------------------------------------------------*
053400170626     C     RTVCIC        BEGSR
053500170626     C*
053600170626     C* Inizializzazioni
053700170627     C                   clear                   wBUFFER
053800170626     C*
053900170626     C* Se indicato uns specifico SUBCODE in input
054000170626     C                   if        ISID4SUBC <> *blanks
054100170627     C                   eval      oO_ICC = oO_DPT + ISID4SUBC
054200170626     C
054300170626     C/EXEC SQL
054400170627     C+ SET :wBUFFER  = (SELECT '1' CONCAT CICICC
054500170627     C+               FROM DPCIC10F
054600170627     C+               WHERE CICATB = ' ' AND CICVER = :sVERSION AND
054700170627     C+               CICICC = :oO_ICC
054800170627     C+               FETCH FIRST 1 ROW ONLY)
054900170626     C/END-EXEC
055000170626     C
055100170626     C*
055200170627     C                   if        %subst(wBUFFER:1:1) = *on
055300170626     C                   else
055400170626     C                   eval      OSID4ERR  = '1'
055500170626     C                   eval      OSID4ERRL = '94'
055600170626     C                   eval      OSID4ERRD = 'Errore in reperimento ' +
055700170626     C                             'INJECTION CODE (DPCIC10F)'
055800170626     C                   seton                                        70
055900170626     C                   endif
056000170626     C*
056100170626     C                   endif
056200170626     C*
056300170626     C                   ENDSR
056400170626
056500170626
056600170626     C*------------------------------------------------------------------------*
056700170626     C* RTVCR1 - Esecuzione ricerca R1
056800170626     C*------------------------------------------------------------------------*
056900170626     C     RTVCR1        BEGSR
057000170626     C*
057100170626     C* Inizializzazioni
057200170626     C                   clear                   oD_BUCN
057300170626     C                   clear                   oD_BUCA
057400170628     C                   clear                   oD_NTWC
057500170628     C                   clear                   oD_ISON
057600170626     C                   clear                   stringaSQL
057700170628     C                   clear                   wNotFoundCR1
057800170626     C*
057900170626     C* Compongo la stringa SQL
058000170626       stringaSQL =
058100170626       'SELECT CR1BUCN FROM DPCR110F ' +
058200170626       'WHERE CR1ATB=§ § and CR1VER='+%editc(sVERSION:'4')+' and ' +
058300170627       'CR1TRADS=§'+ISID4TRADS+'§ and ' +
058400170626       'CR1ISO2=§'+%trim(oD_ISO2)+'§ and (CR1PTCB = § § or ' +
058500170626       '§'+%trim(USID4CAD)+'§ between CR1PTCB and CR1PTCE) and ' +
058600170706       '(CR1NTWP=§ § or CR1NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
058700170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
058800170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
058900170626       ' (CR1ROUT=§ § and CR1ROUPB=§ §) or' +
059000170627       ' (CR1ROUT=§I§ and §'+oO_ICC+'§ between CR1ROUPB and CR1ROUPE) or'+
059100170627       ' (CR1ROUT=§C§ and CR1ROUPB=§'+oO_ISO2+'§) or ' +
059200170627       '(CR1ROUT=§D§ and §'+oO_DPT+'§ between CR1ROUPB and CR1ROUPE) or'+
059300170626       ' (CR1ROUT=§G§ and CR1ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
059400170627       ' (CR1ROUT=§B§ and CR1ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
059500170626       ') ' +
059600170626       'ORDER BY CR1PTY DESC ' +
059700170627       'FETCH FIRST 1 ROW ONLY ';
059800170627
059900170626     C*
060000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
060100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
060200170626     C
060300170626     C* Quindi eseguo stringa SQL così ottenuta
060400170626     C*
060500170626     C/EXEC SQL
060600170626     C+ prepare S_R1 from :stringaSQL
060700170626     C/END-EXEC
060800170626     C
060900170626     C/EXEC SQL
061000170626     C+ declare C_R1 asensitive cursor for S_R1
061100170626     C/END-EXEC
061200170626     C
061300170626     C/EXEC SQL
061400170626     C+ open C_R1
061500170626     C/END-EXEC
061600170626     C
061700170626     C/EXEC SQL
061800170626     C+ Fetch C_R1 into :oD_BUCN
061900170626     C/END-EXEC
062000170626     C*
062100170626     C                   if        sqlcod = *zeros
062200170626     C*
062300170626     C* Quindi aggancio i dati della DESTINATION BUSINESS UNIT
062400170626     C
062500170626     C/EXEC SQL
062600170626     C+ SET :oD_BUCA = (SELECT CBUBUCA FROM DPCBU10F
062700170626     C+                 WHERE CBUATB = ' ' AND CBUVER = :sVERSION AND
062800170627     C+                 CBUBUCN = :oD_BUCN
062900170627     C+                 FETCH FIRST 1 ROW ONLY)
063000170626     C/END-EXEC
063100170626     C                   if        sqlcod <> *zeros
063200170628     C                   eval      wNotFoundCR1 = '1'
063300170626     C                   endif
063400170626     C*
063500170626     C* Quindi aggancio i dati del DESTINATION NETWORK CODE
063600170626     C
063700170626     C/EXEC SQL
063800170626     C+ SET :oD_NTWC = (SELECT CNTNTWC FROM DPCNT10F
063900170626     C+                 WHERE CNTATB = ' ' AND CNTVER = :sVERSION AND
064000170626     C+                 CNTISO2 = :oD_ISO2 AND CNTBUCN = :oD_BUCN
064100170626     C+                 FETCH FIRST 1 ROW ONLY)
064200170626     C/END-EXEC
064300170628     C                   if        sqlcod = *zeros
064400170628     C                   else
064500170628     C*
064600170628     C* Quindi aggancio i dati della NAZIONE
064700170628     C
064800170628     C/EXEC SQL
064900170628     C+ SET :oD_ISON = (SELECT CCCISON FROM DPCCC10F
065000170628     C+                 WHERE CCCATB = ' ' AND CCCVER = :sVERSION AND
065100170628     C+                 CCCISO2 = :oD_ISO2
065200170628     C+                 FETCH FIRST 1 ROW ONLY)
065300170628     C/END-EXEC
065400170628     C                   if        sqlcod = *zeros
065500170628     C                   eval      oD_NTWC = %editc(oD_ISON:'X')
065600170628     C                   else
065700170628     C                   eval      wNotFoundCR1 = '1'
065800170626     C                   endif
065900170628     C                   endif
066000170626     C*
066100170626     C                   else
066200170628     C                   eval      wNotFoundCR1 = '1'
066300170626     C                   endif
066400170626     C
066500170626     C/EXEC SQL
066600170626     C+ close C_R1
066700170626     C/END-EXEC
066800170626     C
066900170626     C*
067000170628     C***                if        wNotFoundCR1 = *on
067100170627     C***                eval      OSID4ERR  = '1'
067200170627     C***                eval      OSID4ERRL = '96'
067300170627     C***                eval      OSID4ERRD = 'Errore in calcolo ' +
067400170627     C***                          'instradamento R1 (DPCR110F)'
067500170627     C***                seton                                        70
067600170627     C***                endif
067700170626     C*
067800170626     C                   ENDSR
067900170626
068000170626
068100170626     C*------------------------------------------------------------------------*
068200170626     C* RTVCR2 - Esecuzione ricerca R2
068300170626     C*------------------------------------------------------------------------*
068400170626     C     RTVCR2        BEGSR
068500170626     C*
068600170626     C* Inizializzazioni
068700170626     C                   clear                   oD_DPT
068800170626     C                   clear                   oD_DPTISO2
068900170626     C                   clear                   oD_DSTR
069000170626     C                   clear                   oD_SSORT
069100170626     C                   clear                   oD_PTNC
069200170627     C                   clear                   oD_DPTIATA
069300170626     C                   clear                   stringaSQL
069400170628     C                   clear                   wNotFoundCR2
069500170627     C                   clear                   DS_DCDP
069600170627     C                   clear                   wBUFFER
069700170626     C*
069800170626     C* Compongo la stringa SQL
069900170626       stringaSQL =
070000170626       'SELECT CR2DDPT, CR2SSORT, CR2PTNC FROM DPCR210F ' +
070100170626       'WHERE CR2ATB=§ § and CR2VER='+%editc(sVERSION:'4')+' and ' +
070200170626       'CR2TRADS=§'+%trim(ISID4TRADS)+'§ and ' +
070300170626       'CR2BUCN='+%editc(oD_BUCN:'X')+' and ' +
070400170626       'CR2ISO2=§'+%trim(oD_ISO2)+'§ and (CR2PTCB = § § or ' +
070500170626       '§'+%trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE) and ' +
070600170706       '(CR2NTWP=§ § or CR2NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
070700170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
070800170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
070900170626       ' (CR2ROUT=§ § and CR2ROUPB=§ §) or' +
071000170627       ' (CR2ROUT=§I§ and §'+oO_ICC+'§ between CR2ROUPB and CR2ROUPE) or'+
071100170627       ' (CR2ROUT=§C§ and CR2ROUPB=§'+oO_ISO2+'§) or ' +
071200170627       '(CR2ROUT=§D§ and §'+oO_DPT+'§ between CR2ROUPB and CR2ROUPE) or'+
071300170626       ' (CR2ROUT=§G§ and CR2ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
071400170627       ' (CR2ROUT=§B§ and CR2ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
071500170626       ') ' +
071600170626       'ORDER BY CR2PTY DESC ' +
071700170627       'FETCH FIRST 1 ROW ONLY ';
071800170627
071900170626     C*
072000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
072100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
072200170626     C
072300170626     C* Quindi eseguo stringa SQL così ottenuta
072400170626     C*
072500170626     C/EXEC SQL
072600170626     C+ prepare S_R2 from :stringaSQL
072700170626     C/END-EXEC
072800170626     C
072900170626     C/EXEC SQL
073000170626     C+ declare C_R2 asensitive cursor for S_R2
073100170626     C/END-EXEC
073200170626     C
073300170626     C/EXEC SQL
073400170626     C+ open C_R2
073500170626     C/END-EXEC
073600170626     C
073700170626     C/EXEC SQL
073800170626     C+ Fetch C_R2 into :oD_DPT, :oD_SSORT, :oD_PTNC
073900170626     C/END-EXEC
074000170626     C*
074100170626     C                   if        sqlcod = *zeros
074200170626     C*
074300170626     C* Quindi aggancio i dati del DESTINATION DEPOT
074400170626     C
074500170627     C/EXEC SQL
074600170627     C+ SET :wBUFFER  = (SELECT
074700170627     C+               '1' CONCAT CDPISO2 CONCAT CDPDSTR CONCAT CDPIATA
074800170629     C+               CONCAT CDPGRPID
074900170627     C+               FROM DPCDP10F
075000170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
075100170627     C+               CDPDPTN = :oD_DPT
075200170627     C+               FETCH FIRST 1 ROW ONLY)
075300170626     C/END-EXEC
075400170626     C
075500170626     C*
075600170627     C                   if        %subst(wBUFFER:1:1) = *on
075700170628     C                   eval      DS_DCDP    = %subst(wBUFFER:2)
075800170627     C                   eval      oD_DPTISO2 = DS_DCDP.oD_DPTISO2
075900170627     C                   eval      oD_DSTR    = DS_DCDP.oD_DSTR
076000170627     C                   eval      oD_DPTIATA = DS_DCDP.oD_DPTIATA
076100170629     C                   eval      oD_GRPL    = DS_DCDP.oD_GRPL
076200170626     C                   else
076300170626     C                   eval      oD_DPTISO2 = oD_ISO2
076400170626     C                   endif
076500170626     C*
076600170626     C                   else
076700170628     C                   eval      wNotFoundCR2 = '1'
076800170626     C                   endif
076900170626     C
077000170626     C/EXEC SQL
077100170626     C+ close C_R2
077200170626     C/END-EXEC
077300170626     C
077400170626     C*
077500170628     C                   if        wNotFoundCR2 = *on
077600170626     C                   eval      OSID4ERR  = '1'
077700170626     C                   eval      OSID4ERRL = '96'
077800170626     C                   eval      OSID4ERRD = 'Errore in calcolo ' +
077900170626     C                             'instradamento R2 (DPCR210F)'
078000170626     C                   seton                                        70
078100170626     C                   endif
078200170626     C*
078300170626     C                   ENDSR
078400170626
078500170626
078600170626     C*------------------------------------------------------------------------*
078700170626     C* RTVCR3 - Esecuzione ricerca R3
078800170626     C*------------------------------------------------------------------------*
078900170626     C     RTVCR3        BEGSR
079000170626     C*
079100170626     C* Inizializzazioni
079200170626     C                   clear                   oD_OSORT
079300170626     C                   clear                   stringaSQL
079400170628     C                   clear                   wNotFoundCR3
079500170626     C*
079600170626     C* Compongo la stringa SQL
079700170626       stringaSQL =
079800170626       'SELECT CR3OSORT FROM DPCR310F ' +
079900170626       'WHERE CR3ATB=§ § and CR3VER='+%editc(sVERSION:'4')+' and ' +
080000170626       'CR3ISO2=§'+%trim(oD_ISO2)+'§ and ' +
080100170626       '(CR3DDPT=§ § or CR3DDPT=§'+oD_DPT+'§) and ' +
080200170626       '(CR3PTCB=§ § or ' +
080300170626       '§'+%trim(USID4CAD)+'§ between CR3PTCB and CR3PTCE) and ' +
080400170706       '(CR3NTWP=§ § or CR3NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
080500170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
080600170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
080700170626       ' (CR3ROUT=§ § and CR3ROUPB=§ §) or' +
080800170627       ' (CR3ROUT=§I§ and §'+oO_ICC+'§ between CR3ROUPB and CR3ROUPE) or'+
080900170627       ' (CR3ROUT=§C§ and CR3ROUPB=§'+oO_ISO2+'§) or ' +
081000170627       '(CR3ROUT=§D§ and §'+oO_DPT+'§ between CR3ROUPB and CR3ROUPE) or'+
081100170626       ' (CR3ROUT=§G§ and CR3ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
081200170627       ' (CR3ROUT=§B§ and CR3ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
081300170626       ') ' +
081400170626       'ORDER BY CR3PTY DESC ' +
081500170627       'FETCH FIRST 1 ROW ONLY ';
081600170627
081700170626     C*
081800170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
081900170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
082000170626     C
082100170626     C* Quindi eseguo stringa SQL così ottenuta
082200170626     C*
082300170626     C/EXEC SQL
082400170626     C+ prepare S_R3 from :stringaSQL
082500170626     C/END-EXEC
082600170626     C
082700170626     C/EXEC SQL
082800170626     C+ declare C_R3 asensitive cursor for S_R3
082900170626     C/END-EXEC
083000170626     C
083100170626     C/EXEC SQL
083200170626     C+ open C_R3
083300170626     C/END-EXEC
083400170626     C
083500170626     C/EXEC SQL
083600170626     C+ Fetch C_R3 into :oD_OSORT
083700170626     C/END-EXEC
083800170626     C*
083900170626     C                   if        sqlcod = *zeros
084000170626     C                   else
084100170628     C                   eval      wNotFoundCR3 = '1'
084200170626     C                   endif
084300170626     C
084400170626     C/EXEC SQL
084500170626     C+ close C_R3
084600170626     C/END-EXEC
084700170627     C
084800170626     C*
084900170626     C                   ENDSR
085000170627
085100170627
085200170627     C*------------------------------------------------------------------------*
085300170627     C* CHKALW - Verifica instradamenti consentiti ALLOW
085400170627     C*------------------------------------------------------------------------*
085500170627     C     CHKALW        BEGSR
085600170627     C*
085700170627     C* Inizializzazioni
085800170627     C                   clear                   oALLOWID
085900170627     C                   clear                   stringaSQL
086000170627     C                   clear                   wAllow
086100170627     C*
086200170627     C* Compongo la stringa SQL
086300170627       stringaSQL =
086400170627       'SELECT CWSALWID FROM DPCWS10F ' +
086500170627       'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
086600170629       '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
086700170724       // '(CWSRULFT=§N§ and CWSRULFB=§'+oO_GRPL+'§) or ' +     //
086800170629       '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
086900170629       '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
087000170629       '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
087100170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
087200170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and (' +
087300170629       %editc(ISID4SORC:'X')+' between CWSRULSB and CWSRULSE or ' +
087400170629       'CWSRULSB=0) and (' +
087500170629       '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
087600170629       '(CWSRULTT=§C§ and CWSRULTB=§'+oD_DPTISO2+'§) or ' +
087700170629       '(CWSRULTT=§N§ and CWSRULTB=§'+oD_NTWC+'§) or ' +
087800170629       '(CWSRULTT=§B§ and CWSRULTB=§'+%editc(oD_BUCN:'X')+oD_DPTISO2+'§) or ' +
087900170724       '(CWSRULTT=§G§ and CWSRULTB in (§'+%trim(oD_GRPL)+'§)) ) and (' +
088000170724       '(§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWSZONTB and CWSZONTE or ' +
088100170724       '(CWSZONTB=§ § and CWSZONTE=§ §))) ' +
088200170629
088300170627       'FETCH FIRST 1 ROW ONLY ';
088400170627
088500170627     C*
088600170627     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
088700170627     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
088800170627     C
088900170627     C* Quindi eseguo stringa SQL così ottenuta
089000170627     C*
089100170627     C/EXEC SQL
089200170627     C+ prepare S_WS from :stringaSQL
089300170627     C/END-EXEC
089400170627     C
089500170627     C/EXEC SQL
089600170627     C+ declare C_WS asensitive cursor for S_WS
089700170627     C/END-EXEC
089800170627     C
089900170627     C/EXEC SQL
090000170627     C+ open C_WS
090100170627     C/END-EXEC
090200170627     C
090300170627     C/EXEC SQL
090400170627     C+ Fetch C_WS into :oALLOWID
090500170627     C/END-EXEC
090600170627     C*
090700170627     C                   if        sqlcod = *zeros
090800170627     C                   eval      wAllow = '1'
090900170627     C                   endif
091000170627     C
091100170627     C/EXEC SQL
091200170627     C+ close C_WS
091300170627     C/END-EXEC
091400170627     C
091500170627     C                   if        wAllow = *on
091600170629     C*
091700170629     C* Se richiesto anche un ADDITIONAL SERVICE CODE => verifico se ALLOW
091800170629     C                   if        ISID4ASCO <> *blanks
091900170629     C                   exsr      CHKCWA
092000170629     C                   endif
092100170627     C                   else
092200170627     C                   eval      OSID4ERR  = '1'
092300170627     C                   eval      OSID4ERRL = '97'
092400170627     C                   eval      OSID4ERRD = 'Instradamento non ' +
092500170627     C                             'consentito. (DPCWS10F)'
092600170627     C                   seton                                        70
092700170627     C                   endif
092800170627     C*
092900170627     C                   ENDSR
093000170629
093100170629
093200170629     C*------------------------------------------------------------------------*
093300170629     C* CHKCWA - Verifica instradamenti consentiti ALLOW - ASCODE
093400170629     C*------------------------------------------------------------------------*
093500170629     C     CHKCWA        BEGSR
093600170629     C*
093700170629     C* Inizializzazioni
093800170629     C                   clear                   stringaSQL
093900170629     C                   clear                   wFound
094000170629     C*
094100170629     C* Compongo la stringa SQL
094200170629       stringaSQL =
094300170629       'SELECT §1§ FROM DPCWA10F ' +
094400170629       'WHERE CWAATB=§ § and CWAVER='+%editc(sVERSION:'4')+' and ' +
094500170629       'CWAALWID='+%editc(oALLOWID:'4')+' and (§' +
094600170629       ISID4ASCO+'§ between CWARULSB and CWARULSE or CWARULSB=§ §) and (' +
094700170629       '§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWAZONTB and CWAZONTE)) ' +
094800170629
094900170629       'FETCH FIRST 1 ROW ONLY ';
095000170629
095100170629     C*
095200170629     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
095300170629     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
095400170629     C
095500170629     C* Quindi eseguo stringa SQL così ottenuta
095600170629     C*
095700170629     C/EXEC SQL
095800170629     C+ prepare S_WA from :stringaSQL
095900170629     C/END-EXEC
096000170629     C
096100170629     C/EXEC SQL
096200170629     C+ declare C_WA asensitive cursor for S_WA
096300170629     C/END-EXEC
096400170629     C
096500170629     C/EXEC SQL
096600170629     C+ open C_WA
096700170629     C/END-EXEC
096800170629     C
096900170629     C/EXEC SQL
097000170629     C+ Fetch C_WS into :wFound
097100170629     C/END-EXEC
097200170629     C*
097300170629     C                   if        wFound = *on
097400170629     C                   else
097500170629     C                   eval      OSID4ERR  = '1'
097600170629     C                   eval      OSID4ERRL = '97'
097700170629     C                   eval      OSID4ERRD = 'Instradamento non ' +
097800170629     C                             'consentito. (DPCWA10F)'
097900170629     C                   seton                                        70
098000170629     C                   endif
098100170629     C
098200170629     C/EXEC SQL
098300170629     C+ close C_WS
098400170629     C/END-EXEC
098500170629     C
098600170629     C*
098700170629     C                   ENDSR
098800170626
098900170626
099000170626     C*------------------------------------------------------------------------*
099100170626     C* RTVCR4 - Esecuzione ricerca R4
099200170626     C*------------------------------------------------------------------------*
099300170626     C     RTVCR4        BEGSR
099400170626     C*
099500170626     C* Inizializzazioni
099600170626     C                   clear                   oD_CSORT
099700170626     C                   clear                   stringaSQL
099800170628     C                   clear                   wNotFoundCR4
099900170626     C*
100000170626     C* Compongo la stringa SQL
100100170626       stringaSQL =
100200170627       'SELECT CR4CSORT FROM DPCR410F ' +
100300170626       'WHERE CR4ATB=§ § and CR4VER='+%editc(sVERSION:'4')+' and ' +
100400170626       'CR4ISO2=§'+%trim(oD_ISO2)+'§ and ' +
100500170626       '(CR4DDPT=§ § or CR4DDPT=§'+oD_DPT+'§) and ' +
100600170706       '(CR4NTWP=§ § or CR4NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
100700170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
100800170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
100900170626       ' (CR4ROUT=§ § and CR4ROUPB=§ §) or' +
101000170627       ' (CR4ROUT=§I§ and §'+oO_ICC+'§ between CR4ROUPB and CR4ROUPE) or'+
101100170627       ' (CR4ROUT=§C§ and CR4ROUPB=§'+oO_ISO2+'§) or ' +
101200170627       '(CR4ROUT=§D§ and §'+oO_DPT+'§ between CR4ROUPB and CR4ROUPE) or'+
101300170626       ' (CR4ROUT=§G§ and CR4ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
101400170629       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
101500170627       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
101600170626       ') ' +
101700170626       'ORDER BY CR4PTY DESC ' +
101800170627       'FETCH FIRST 1 ROW ONLY ';
101900170627
102000170626     C*
102100170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
102200170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
102300170626     C
102400170626     C* Quindi eseguo stringa SQL così ottenuta
102500170626     C*
102600170626     C/EXEC SQL
102700170626     C+ prepare S_R4 from :stringaSQL
102800170626     C/END-EXEC
102900170626     C
103000170626     C/EXEC SQL
103100170626     C+ declare C_R4 asensitive cursor for S_R4
103200170626     C/END-EXEC
103300170626     C
103400170626     C/EXEC SQL
103500170626     C+ open C_R4
103600170626     C/END-EXEC
103700170626     C
103800170626     C/EXEC SQL
103900170626     C+ Fetch C_R4 into :oD_CSORT
104000170626     C/END-EXEC
104100170626     C*
104200170626     C                   if        sqlcod = *zeros
104300170626     C                   else
104400170628     C                   eval      wNotFoundCR4 = '1'
104500170626     C                   endif
104600170626     C
104700170626     C/EXEC SQL
104800170626     C+ close C_R4
104900170626     C/END-EXEC
105000170626     C
105100170626     C*
105200170626     C                   ENDSR
105300170626
105400170626
105500170626     C*------------------------------------------------------------------------*
105600170626     C* RTVCR5 - Esecuzione ricerca R5
105700170626     C*------------------------------------------------------------------------*
105800170626     C     RTVCR5        BEGSR
105900170626     C*
106000170626     C* Inizializzazioni
106100170626     C                   clear                   oD_DSORT
106200170626     C                   clear                   stringaSQL
106300170628     C                   clear                   wNotFoundCR5
106400170626     C*
106500170626     C* Compongo la stringa SQL
106600170626       stringaSQL =
106700170627       'SELECT CR5DSORT FROM DPCR510F ' +
106800170626       'WHERE CR5ATB=§ § and CR5VER='+%editc(sVERSION:'4')+' and ' +
106900170724       'CR5ISO2=§'+%trim(oD_ISO2)+'§ and ' +
107000170627       'CR5DDPT=§'+oD_DPT+'§ and ' +
107100170626       '(CR5PTCB=§ § or ' +
107200170626       '§'+%trim(USID4CAD)+'§ between CR5PTCB and CR5PTCE) and ' +
107300170706       '(CR5NTWP=§ § or CR5NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
107400170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
107500170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
107600170626       ' (CR5ROUT=§ § and CR5ROUPB=§ §) or' +
107700170627       ' (CR5ROUT=§I§ and §'+oO_ICC+'§ between CR5ROUPB and CR5ROUPE) or'+
107800170627       ' (CR5ROUT=§C§ and CR5ROUPB=§'+oO_ISO2+'§) or ' +
107900170627       '(CR5ROUT=§D§ and §'+oO_DPT+'§ between CR5ROUPB and CR5ROUPE) or'+
108000170626       ' (CR5ROUT=§G§ and CR5ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
108100170629       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
108200170627       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
108300170626       ') ' +
108400170626       'ORDER BY CR5PTY DESC ' +
108500170627       'FETCH FIRST 1 ROW ONLY ';
108600170627
108700170626     C*
108800170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
108900170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
109000170626     C
109100170626     C* Quindi eseguo stringa SQL così ottenuta
109200170626     C*
109300170626     C/EXEC SQL
109400170626     C+ prepare S_R5 from :stringaSQL
109500170626     C/END-EXEC
109600170626     C
109700170626     C/EXEC SQL
109800170626     C+ declare C_R5 asensitive cursor for S_R5
109900170626     C/END-EXEC
110000170626     C
110100170626     C/EXEC SQL
110200170626     C+ open C_R5
110300170626     C/END-EXEC
110400170626     C
110500170626     C/EXEC SQL
110600170626     C+ Fetch C_R5 into :oD_DSORT
110700170626     C/END-EXEC
110800170626     C*
110900170626     C                   if        sqlcod = *zeros
111000170626     C                   else
111100170628     C                   eval      wNotFoundCR5 = '1'
111200170626     C                   endif
111300170626     C
111400170626     C/EXEC SQL
111500170626     C+ close C_R5
111600170626     C/END-EXEC
111700170626     C
111800170626     C*
111900170626     C                   ENDSR
112000170627
112100170627
112200170627     C*------------------------------------------------------------------------*
112300170627     C* EXEOUT - Valorizzo paramteri di output
112400170627     C*------------------------------------------------------------------------*
112500170627     C     EXEOUT        BEGSR
112600170627     C*
112700170627     C                   eval      OSID4VER   = sVERSION
112800170627     C                   eval      OSID4VERD  = oVER_DPD
112900170627     C                   eval      OSID4VDDE  = CVEDDE
113000170627     C                   eval      OSID4DBUCN = oD_BUCN
113100170627     C                   eval      OSID4DBUCA = oD_BUCA
113200170627     C                   eval      OSID4DNTWC = oD_NTWC
113300170627     C                   eval      OSID4DDPTC = oD_DPTISO2
113400170627     C                   eval      OSID4DDPT  = oD_DPT
113500170627     C                   eval      OSID4DSTR  = oD_DSTR
113600170627     C                   eval      OSID4SSORT = oD_SSORT
113700170627     C                   eval      OSID4DPTNC = oD_PTNC
113800170627     C                   eval      OSID4OSORT = oD_OSORT
113900170627     C                   eval      OSID4CSORT = oD_CSORT
114000170627     C                   eval      OSID4DSORT = oD_DSORT
114100170627     C                   eval      OSID4BCID  = oVER_BCID
114200170627     C                   eval      OSID4SRVX  = oSRV_TXT
114300170627     C                   eval      OSID4SRVM  = oSRV_MRK
114400170627     C                   eval      OSID4ODPT  = oO_DPT
114500170627     C                   eval      OSID4ODPTC = oO_ISO2
114600170627     C                   eval      OSID4OBUCN = oO_BUCN
114700170627     C                   eval      OSID4OGRPL = oO_GRPL
114800170627     C                   eval      OSID4OICC  = oO_ICC
114900170627     C                   eval      OSID4ALWID = oALLOWID
115000170627     C                   eval      OSID4IATA  = oD_DPTIATA
115100170629     C                   evalr     OSID4CADP  = %trim(USID4CAD)
115200170629     C                   eval      OSID4CADP = %scanrpl(' ':'0':OSID4CADP)
115300170627     C*
115400170627     C* Sancisco esito a OK
115500170627     C                   movel     *blanks       OSID4ERR
115600170627     C*
115700170627     C                   ENDSR
115800170626
115900170626
116000170626     C*------------------------------------------------------------------------*
116100170626     C* CARTAB - Caricamemto tabelle si procedura
116200170626     C*------------------------------------------------------------------------*
116300170626     C     CARTAB        BEGSR
116400170626     C*
116500170626     C* Reperisco i valori tabellati di procedura
116600170626     C                   eval      tblKUT = 1
116700170626     C                   eval      tblCOD = '3I'
116800170626     C                   eval      tblKEY ='DPD'
116900170626     C     KEYtab00_C    chain     tabel00f
117000170626     C                   if        %found(tabel00f)
117100170626     C                   eval      DS3IDP = tblUNI
117200170627     C                   eval      gBU   = §3IBUCN
117300170627     C                   eval      gISO2 = §3IISO2
117400170626     C                   else
117500170626     C                   eval      OSID4ERR  = '1'
117600170627     C                   eval      OSID4ERRL = '80'
117700170627     C                   eval      OSID4ERRD = 'Tabella 3I valore §3IBUC non ' +
117800170626     C                             'trovato'
117900170626     C                   seton                                        70
118000170626     C                   endif
118100170626     C*
118200170626     C                   ENDSR
118300170620
118400170620
118500170620     C*--------------------------------------------------------------------------------------------*
118600060515     C* *inzsr - operazioni iniziali
118700060515     C*--------------------------------------------------------------------------------------------*
118800000000     C     *inzsr        BEGSR
118900060515     C*
119000060515     C* Ricevimento parametri
119100060515     C     *ENTRY        PLIST
119200170627     C                   PARM                    TISID4DS
119300060510     C*
119400060510     C* CALCOLA LA DATA CORRENTE
119500170620     C                   Z-ADD     *zeros        datcor            8 0
119600170620     C                   EVAL      datcor = %dec(%date() : *iso)
119700170626     C*
119800170626     C* CARICAMENTO TABELLE DI PROCEDURA
119900170626     C                   EXSR      CARTAB
120000170626     C*
120100170626     C* TABEL00F - Completa
120200170626     C     KEYtab00_C    KLIST
120300170626     C                   KFLD                    tblKUT
120400170626     C                   KFLD                    tblCOD
120500170626     C                   KFLD                    tblKEY
120600170622     C*
120700170622     C* DPCCC14I - Completa
120800170622     C     KEYccc14_C    KLIST
120900170622     C                   KFLD                    cccATB
121000170622     C                   KFLD                    cccVER
121100170622     C                   KFLD                    cccBRT
121200060515     C*
121300060515     C                   ENDSR
