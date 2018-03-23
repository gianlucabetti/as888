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
027500180108     C                             and ISID4CAD <>'2'
027600170629     C                   eval      USID4CAD = '1'
027700170629     C                   endif
027800170622     C*
027900170622     C* Servizio DPD
028000170622     C                   if        ISID4SORC = *zeros
028100170622     C                   seton                                        40
028200170622     C                   endif
028300170622     C*
028400170622     C* Se nn ok => imposto subito l'errore generale d procedura
028500170622     C                   if        *in40
028600170622     C                   eval      OSID4ERR  = '1'
028700170622     C                   eval      OSID4ERRL = '92'
028800170622     C                   eval      OSID4ERRD = 'Errore: parametri obbligatori '+
028900170622     C                             'non valorizzati'
029000170622     C                   seton                                        70
029100170622     C                   endif
029200170622     C*
029300170622     C                   ENDSR
029400170622
029500170622
029600170622     C*------------------------------------------------------------------------*
029700170622     C* RTVVER - Reperimento VERSIONE richiesta
029800170622     C*------------------------------------------------------------------------*
029900170622     C     RTVVER        BEGSR
030000170622     C*
030100170622     C* Reperisco VERSIONE solo se non ancora reperita o data riferimento cambiata
030200170626     C                   if        sVERSION  = *zeros    OR
030300170626     C                             ISID4DRI <> sDAT_RIF
030400170626     C*
030500170626     C                   eval      sDAT_RIF = ISID4DRI
030600170622     C*
030700170622     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
030800170627     C                   call      'TISID1R'
030900170626     C                   parm                    sDAT_RIF
031000170626     C                   parm                    sVERSION
031100170627     C                   parm                    oVER_DPD
031200170627     C                   parm                    CVEDDE
031300170627     C                   parm                    CVEDSC
031400170626     C                   parm                    oVER_BCID
031500170622     C*
031600170622     C* Se reperita versione valida
031700170626     C                   if        sVERSION  > *zeros
031800170622     C                   else
031900170622     C*
032000170622     C* Se non ok => imposto subito l'errore generale d procedura
032100170622     C                   eval      OSID4ERR  = '1'
032200170622     C                   eval      OSID4ERRL = '90'
032300170622     C                   eval      OSID4ERRD = 'Errore in reperimento versione'+
032400170622     C                             ' (DPCVE10F)'
032500170622     C                   seton                                        70
032600170622     C                   endif
032700170622     C                   endif
032800170622     C*
032900170622     C                   ENDSR
033000170622
033100170622
033200170622     C*------------------------------------------------------------------------*
033300170626     C* RTVDPT - Reperimento DEPOT DPD (partenza)
033400170622     C*------------------------------------------------------------------------*
033500170626     C     RTVDPT        BEGSR
033600170622     C*
033700170626     C                   clear                   oO_DPT
033800170622     C*
033900170622     C* Cerco abbinamento Filiale BRT <=> Depot DPD
034000170627     C                   z-add     1             jLNP
034100170622     C     ISID4LNP      lookup    SkLNP(jLNP)                            55
034200170622     C                   if        %found
034300170626     C                   eval      oO_DPT = SkDPT(jLNP)
034400170622     C                   else
034500170622     C*
034600170622     C     ISID4LNP      chain     azorg01l
034700170622     C                   if        %found(azorg01l)
034800170622     C                   eval      OG143 = ORGDE3
034900170622     C                   add       1             sjLNP
035000170626     C                   eval      SkLNP(sjLNP) = ISID4LNP
035100170627     C                   eval      SkDPT(sjLNP) = %editc(gBU:'X')+§OGDP1
035200170626     C                   eval      oO_DPT = SkDPT(sjLNP)
035300170622     C                   endif
035400170622     C*
035500170626     C                   if        oO_DPT = *blanks
035600170622     C                   eval      OSID4ERR  = '1'
035700170622     C                   eval      OSID4ERRL = '91'
035800170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
035900170622     C                             'partenza DPD (AZORG01L)'
036000170622     C                   seton                                        70
036100170622     C                   endif
036200170622     C                   endif
036300170622     C*
036400170622     C                   ENDSR
036500170622
036600170622
036700170622     C*------------------------------------------------------------------------*
036800170626     C* RTVDISO2 - Reperimento DESTINATION CODICE NAZIONE ISO 2
036900170622     C*------------------------------------------------------------------------*
037000170626     C     RTVDISO2      BEGSR
037100170622     C*
037200170622     C                   clear                   oD_ISO2
037300170622     C*
037400170622     C* Cerco abbinamento Cod Nazione BRT <=> Codice Nazione ISO2
037500170627     C                   z-add     1             jNZD
037600170622     C     ISID4NZD      lookup    SkNZD(jNZD)                            55
037700170913     C                   if        %found
037800170630     C                   eval      oD_ISO2 = SkISO2(jNZD)
037900170622     C                   else
038000170622     C*
038100170622     C                   eval      cccATB = *blanks
038200170626     C                   eval      cccVER = sVERSION
038300170622     C                   eval      cccBRT = ISID4NZD
038400170622     C     KEYccc14_C    chain     dpccc14i
038500170622     C                   if        %found(dpccc14i)
038600170622     C                   add       1             sjNZD
038700170622     C                   eval      SkNZD(sjNZD)  = ISID4NZD
038800170622     C                   eval      SkISO2(sjNZD) = cccISO2
038900170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
039000170622     C                   endif
039100170622     C*
039200170622     C                   if        oD_ISO2 = *blanks
039300170622     C                   eval      OSID4ERR  = '1'
039400170622     C                   eval      OSID4ERRL = '93'
039500170622     C                   eval      OSID4ERRD = 'Errore in reperimento nazione '+
039600170622     C                             'destino (DPCCC10F)'
039700170622     C                   seton                                        70
039800170622     C                   endif
039900170622     C                   endif
040000170622     C*
040100170622     C                   ENDSR
040200170620
040300170620
040400170620     C*------------------------------------------------------------------------*
040500170620     C* CHKPATT - Verifica del Pattern CAP
040600170620     C*------------------------------------------------------------------------*
040700170620     C     CHKPATT       BEGSR
040800170622     C*
040900170622     C* Inizializzazioni
041000170622     C                   clear                   TISID2DS
041100170622     C*
041200170622     C* Valorizzazioni
041300170627     C                   eval      ISID2VER   = sVERSION
041400170627     C                   eval      ISID2NISO2 = oD_ISO2
041500170629     C                   eval      ISID2PTC   = USID4CAD
041600170622     C                   call      'TISID2R'
041700170622     C                   parm                    TISID2DS
041800170622     C*
041900170622     C                   if        OSID2ESI  = 'V'
042000170622     C                   eval      USID4CAD = OSID2PTC
042100170622     C                   else
042200170622     C                   eval      OSID4ERR  = '1'
042300170627     C                   eval      OSID4ERRL = '99'
042400170627     C                   eval      OSID4ERRD = 'Errore - CAP errato ' +
042500170622     C                             '(Pattern DPCCC10F)'
042600170622     C                   seton                                        70
042700170622     C                   endif
042800170620     C*
042900170620     C                   ENDSR
043000170622
043100170622
043200170622     C*------------------------------------------------------------------------*
043300170622     C* CHKPTC - Verifica esistenza CAP in DPD
043400170622     C*------------------------------------------------------------------------*
043500170622     C     CHKPTC        BEGSR
043600170622     C*
043700170622     C* Inizializzazioni
043800170622     C                   movel     *blanks       wFound
043900170629     C*
044000170629     C* Se necessaria validazione del CAP rispetto alla NAZIONE
044100170629     C                   if        OSID2PVAL = 'VB'                             * blocked on failure
044200170629     C
044300170622     C/EXEC SQL
044400170622     C+ SET :wFound = (SELECT '1' FROM DPCPC10F WHERE CPCATB = ' ' AND
044500170626     C+                CPCVER = :sVERSION AND CPCISO2 = :oD_ISO2
044600170626     C+                AND :USID4CAD BETWEEN CPCPTCB AND CPCPTCE
044700170626     C+                FETCH FIRST 1 ROW ONLY)
044800170622     C/END-EXEC
044900170622     C
045000170622     C*
045100170622     C                   if        wFound = *on
045200170622     C                   else
045300170622     C                   eval      OSID4ERR  = '1'
045400170630     C                   eval      OSID4ERRL = '96'
045500170627     C                   eval      OSID4ERRD = 'Errore - CAP non trovato ' +
045600170622     C                             '(DPCPC10F)'
045700170622     C                   seton                                        70
045800170622     C                   endif
045900170629     C*
046000170629     C                   endif
046100170622     C*
046200170622     C                   ENDSR
046300170622
046400170622
046500170622     C*------------------------------------------------------------------------*
046600170622     C* RTVSORC - Reperimento dati SOCODE
046700170622     C*------------------------------------------------------------------------*
046800170622     C     RTVSORC       BEGSR
046900170622     C*
047000170622     C* Inizializzazioni
047100170627     C                   clear                   DS_CSO
047200170627     C                   clear                   wBUFFER
047300170622     C
047400170622     C/EXEC SQL
047500170629     C+ SET :wBUFFER = (SELECT '1' CONCAT CSOSTXT CONCAT CSOSMRK FROM DPCSO10F
047600170627     C+              WHERE CSOATB = ' ' AND CSOVER = :sVERSION AND
047700170627     C+              CSOSORC = :ISID4SORC
047800170627     C+              FETCH FIRST 1 ROW ONLY)
047900170622     C/END-EXEC
048000170622     C
048100170622     C*
048200170629     C                   if        %subst(wBUFFER:1:1) = *on
048300170628     C                   eval      DS_CSO   = %subst(wBUFFER:2)
048400170627     C                   eval      oSRV_TXT = DS_CSO.oSRV_TXT
048500170627     C                   eval      oSRV_MRK = DS_CSO.oSRV_MRK
048600170622     C                   else
048700170622     C                   eval      OSID4ERR  = '1'
048800170627     C                   eval      OSID4ERRL = '95'
048900170627     C                   eval      OSID4ERRD = 'Errore in reperimento servizio'+
049000170622     C                             ' (DPCSC10F)'
049100170622     C                   seton                                        70
049200170622     C                   endif
049300170622     C*
049400170622     C                   ENDSR
049500170622
049600170622
049700170622     C*------------------------------------------------------------------------*
049800170626     C* RTVODPT - Reperimento dati ORIGIN DEPOT
049900170622     C*------------------------------------------------------------------------*
050000170626     C     RTVODPT       BEGSR
050100170622     C*
050200170622     C* Inizializzazioni
050300170627     C                   clear                   DS_OCDP
050400170627     C                   clear                   wBUFFER
050500170622     C
050600170622     C/EXEC SQL
050700170627     C+ SET :wBUFFER  = (SELECT
050800170627     C+               '1' CONCAT CDPISO2 CONCAT digits(CDPBUCN) CONCAT CDPGRPID
050900170627     C+               FROM DPCDP10F
051000170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
051100170627     C+               CDPDPTN = :oO_DPT
051200170627     C+               FETCH FIRST 1 ROW ONLY)
051300170622     C/END-EXEC
051400170622     C
051500170622     C*
051600170627     C                   if        %subst(wBUFFER:1:1) = *on
051700170628     C                   eval      DS_OCDP = %subst(wBUFFER:2)
051800170627     C                   eval      oO_ISO2 = DS_OCDP.oO_ISO2
051900170627     C                   eval      oO_BUCN = DS_OCDP.oO_BUCN
052000170627     C                   eval      oO_GRPL = DS_OCDP.oO_GRPL
052100170622     C                   else
052200170622     C                   eval      OSID4ERR  = '1'
052300170626     C                   eval      OSID4ERRL = '91'
052400170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
052500170622     C                             'partenza DPD (DPCDP10F)'
052600170622     C                   seton                                        70
052700170622     C                   endif
052800170622     C*
052900170622     C                   ENDSR
053000170626
053100170626
053200170626     C*------------------------------------------------------------------------*
053300170626     C* RTVCIC - Reperimento dati INJECTION CODE
053400170626     C*------------------------------------------------------------------------*
053500170626     C     RTVCIC        BEGSR
053600170626     C*
053700170626     C* Inizializzazioni
053800170627     C                   clear                   wBUFFER
053900170626     C*
054000170626     C* Se indicato uns specifico SUBCODE in input
054100170626     C                   if        ISID4SUBC <> *blanks
054200170627     C                   eval      oO_ICC = oO_DPT + ISID4SUBC
054300170626     C
054400170626     C/EXEC SQL
054500170627     C+ SET :wBUFFER  = (SELECT '1' CONCAT CICICC
054600170627     C+               FROM DPCIC10F
054700170627     C+               WHERE CICATB = ' ' AND CICVER = :sVERSION AND
054800170627     C+               CICICC = :oO_ICC
054900170627     C+               FETCH FIRST 1 ROW ONLY)
055000170626     C/END-EXEC
055100170626     C
055200170626     C*
055300170627     C                   if        %subst(wBUFFER:1:1) = *on
055400170626     C                   else
055500170626     C                   eval      OSID4ERR  = '1'
055600170626     C                   eval      OSID4ERRL = '94'
055700170626     C                   eval      OSID4ERRD = 'Errore in reperimento ' +
055800170626     C                             'INJECTION CODE (DPCIC10F)'
055900170626     C                   seton                                        70
056000170626     C                   endif
056100170626     C*
056200170626     C                   endif
056300170626     C*
056400170626     C                   ENDSR
056500170626
056600170626
056700170626     C*------------------------------------------------------------------------*
056800170626     C* RTVCR1 - Esecuzione ricerca R1
056900170626     C*------------------------------------------------------------------------*
057000170626     C     RTVCR1        BEGSR
057100170626     C*
057200170626     C* Inizializzazioni
057300170626     C                   clear                   oD_BUCN
057400170626     C                   clear                   oD_BUCA
057500170628     C                   clear                   oD_NTWC
057600170628     C                   clear                   oD_ISON
057700170626     C                   clear                   stringaSQL
057800170628     C                   clear                   wNotFoundCR1
057900170626     C*
058000170626     C* Compongo la stringa SQL
058100170626       stringaSQL =
058200170626       'SELECT CR1BUCN FROM DPCR110F ' +
058300170626       'WHERE CR1ATB=§ § and CR1VER='+%editc(sVERSION:'4')+' and ' +
058400170627       'CR1TRADS=§'+ISID4TRADS+'§ and ' +
058500170626       'CR1ISO2=§'+%trim(oD_ISO2)+'§ and (CR1PTCB = § § or ' +
058600170626       '§'+%trim(USID4CAD)+'§ between CR1PTCB and CR1PTCE) and ' +
058700170706       '(CR1NTWP=§ § or CR1NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
058800170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
058900170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
059000170626       ' (CR1ROUT=§ § and CR1ROUPB=§ §) or' +
059100170627       ' (CR1ROUT=§I§ and §'+oO_ICC+'§ between CR1ROUPB and CR1ROUPE) or'+
059200170627       ' (CR1ROUT=§C§ and CR1ROUPB=§'+oO_ISO2+'§) or ' +
059300170627       '(CR1ROUT=§D§ and §'+oO_DPT+'§ between CR1ROUPB and CR1ROUPE) or'+
059400170626       ' (CR1ROUT=§G§ and CR1ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
059500170627       ' (CR1ROUT=§B§ and CR1ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
059600170626       ') ' +
059700170626       'ORDER BY CR1PTY DESC ' +
059800170627       'FETCH FIRST 1 ROW ONLY ';
059900170627
060000170626     C*
060100170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
060200170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
060300170626     C
060400170626     C* Quindi eseguo stringa SQL così ottenuta
060500170626     C*
060600170626     C/EXEC SQL
060700170626     C+ prepare S_R1 from :stringaSQL
060800170626     C/END-EXEC
060900170626     C
061000170626     C/EXEC SQL
061100170626     C+ declare C_R1 asensitive cursor for S_R1
061200170626     C/END-EXEC
061300170626     C
061400170626     C/EXEC SQL
061500170626     C+ open C_R1
061600170626     C/END-EXEC
061700170626     C
061800170626     C/EXEC SQL
061900170626     C+ Fetch C_R1 into :oD_BUCN
062000170626     C/END-EXEC
062100170626     C*
062200170626     C                   if        sqlcod = *zeros
062300170626     C*
062400170626     C* Quindi aggancio i dati della DESTINATION BUSINESS UNIT
062500170626     C
062600170626     C/EXEC SQL
062700170626     C+ SET :oD_BUCA = (SELECT CBUBUCA FROM DPCBU10F
062800170626     C+                 WHERE CBUATB = ' ' AND CBUVER = :sVERSION AND
062900170627     C+                 CBUBUCN = :oD_BUCN
063000170627     C+                 FETCH FIRST 1 ROW ONLY)
063100170626     C/END-EXEC
063200170626     C                   if        sqlcod <> *zeros
063300170628     C                   eval      wNotFoundCR1 = '1'
063400170626     C                   endif
063500170626     C*
063600170626     C* Quindi aggancio i dati del DESTINATION NETWORK CODE
063700170626     C
063800170626     C/EXEC SQL
063900170626     C+ SET :oD_NTWC = (SELECT CNTNTWC FROM DPCNT10F
064000170626     C+                 WHERE CNTATB = ' ' AND CNTVER = :sVERSION AND
064100170626     C+                 CNTISO2 = :oD_ISO2 AND CNTBUCN = :oD_BUCN
064200170626     C+                 FETCH FIRST 1 ROW ONLY)
064300170626     C/END-EXEC
064400170628     C                   if        sqlcod = *zeros
064500170628     C                   else
064600170628     C*
064700170628     C* Quindi aggancio i dati della NAZIONE
064800170628     C
064900170628     C/EXEC SQL
065000170628     C+ SET :oD_ISON = (SELECT CCCISON FROM DPCCC10F
065100170628     C+                 WHERE CCCATB = ' ' AND CCCVER = :sVERSION AND
065200170628     C+                 CCCISO2 = :oD_ISO2
065300170628     C+                 FETCH FIRST 1 ROW ONLY)
065400170628     C/END-EXEC
065500170628     C                   if        sqlcod = *zeros
065600170628     C                   eval      oD_NTWC = %editc(oD_ISON:'X')
065700170628     C                   else
065800170628     C                   eval      wNotFoundCR1 = '1'
065900170626     C                   endif
066000170628     C                   endif
066100170626     C*
066200170626     C                   else
066300170628     C                   eval      wNotFoundCR1 = '1'
066400170626     C                   endif
066500170626     C
066600170626     C/EXEC SQL
066700170626     C+ close C_R1
066800170626     C/END-EXEC
066900170626     C
067000170626     C*
067100170628     C***                if        wNotFoundCR1 = *on
067200170627     C***                eval      OSID4ERR  = '1'
067300170627     C***                eval      OSID4ERRL = '96'
067400170627     C***                eval      OSID4ERRD = 'Errore in calcolo ' +
067500170627     C***                          'instradamento R1 (DPCR110F)'
067600170627     C***                seton                                        70
067700170627     C***                endif
067800170626     C*
067900170626     C                   ENDSR
068000170626
068100170626
068200170626     C*------------------------------------------------------------------------*
068300170626     C* RTVCR2 - Esecuzione ricerca R2
068400170626     C*------------------------------------------------------------------------*
068500170626     C     RTVCR2        BEGSR
068600170626     C*
068700170626     C* Inizializzazioni
068800170626     C                   clear                   oD_DPT
068900170626     C                   clear                   oD_DPTISO2
069000170626     C                   clear                   oD_DSTR
069100170626     C                   clear                   oD_SSORT
069200170626     C                   clear                   oD_PTNC
069300170627     C                   clear                   oD_DPTIATA
069400170626     C                   clear                   stringaSQL
069500170628     C                   clear                   wNotFoundCR2
069600170627     C                   clear                   DS_DCDP
069700170627     C                   clear                   wBUFFER
069800170626     C*
069900170626     C* Compongo la stringa SQL
070000170626       stringaSQL =
070100170626       'SELECT CR2DDPT, CR2SSORT, CR2PTNC FROM DPCR210F ' +
070200170626       'WHERE CR2ATB=§ § and CR2VER='+%editc(sVERSION:'4')+' and ' +
070300170626       'CR2TRADS=§'+%trim(ISID4TRADS)+'§ and ' +
070400170626       'CR2BUCN='+%editc(oD_BUCN:'X')+' and ' +
070500170626       'CR2ISO2=§'+%trim(oD_ISO2)+'§ and (CR2PTCB = § § or ' +
070600170626       '§'+%trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE) and ' +
070700170706       '(CR2NTWP=§ § or CR2NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
070800170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
070900170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
071000170626       ' (CR2ROUT=§ § and CR2ROUPB=§ §) or' +
071100170627       ' (CR2ROUT=§I§ and §'+oO_ICC+'§ between CR2ROUPB and CR2ROUPE) or'+
071200170627       ' (CR2ROUT=§C§ and CR2ROUPB=§'+oO_ISO2+'§) or ' +
071300170627       '(CR2ROUT=§D§ and §'+oO_DPT+'§ between CR2ROUPB and CR2ROUPE) or'+
071400170626       ' (CR2ROUT=§G§ and CR2ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
071500170627       ' (CR2ROUT=§B§ and CR2ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
071600170626       ') ' +
071700170626       'ORDER BY CR2PTY DESC ' +
071800170627       'FETCH FIRST 1 ROW ONLY ';
071900170627
072000170626     C*
072100170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
072200170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
072300170626     C
072400170626     C* Quindi eseguo stringa SQL così ottenuta
072500170626     C*
072600170626     C/EXEC SQL
072700170626     C+ prepare S_R2 from :stringaSQL
072800170626     C/END-EXEC
072900170626     C
073000170626     C/EXEC SQL
073100170626     C+ declare C_R2 asensitive cursor for S_R2
073200170626     C/END-EXEC
073300170626     C
073400170626     C/EXEC SQL
073500170626     C+ open C_R2
073600170626     C/END-EXEC
073700170626     C
073800170626     C/EXEC SQL
073900170626     C+ Fetch C_R2 into :oD_DPT, :oD_SSORT, :oD_PTNC
074000170626     C/END-EXEC
074100170626     C*
074200170626     C                   if        sqlcod = *zeros
074300170626     C*
074400170626     C* Quindi aggancio i dati del DESTINATION DEPOT
074500170626     C
074600180104     ***  C/EXEC SQL
074700180104     ***  C+ SET :wBUFFER  = (SELECT
074800180104     ***  C+               '1' CONCAT CDPISO2 CONCAT CDPDSTR CONCAT CDPIATA
074900180104     ***  C+               CONCAT CDPGRPID
075000180104     ***  C+               FROM DPCDP10F
075100180104     ***  C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
075200180104     ***  C+               CDPDPTN = :oD_DPT
075300180104     ***  C+               FETCH FIRST 1 ROW ONLY)
075400180104     ***  C/END-EXEC
075500180104     C*
075600180104     C* poichè molti casi di DEPOT non hanno la NAZIONE ISO2, allora la decodifica
075700180104     C*  dalla B.UNIT
075800180104     C/EXEC SQL
075900180104     C+ SET :wBUFFER  = (SELECT '1' CONCAT
076000180104     c+             CASE WHEN CDPISO2 <> ' ' THEN CDPISO2 ELSE CBUISO2 END
076100180104     C+               CONCAT CDPDSTR CONCAT CDPIATA CONCAT CDPGRPID
076200180104     C+               FROM DPCDP10F join DPCBU10F
076300180104     C+                 on CDPBUCN = CBUBUCN and CDPVER = CBUVER
076400180104     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
076500180104     C+               CDPDPTN = :oD_DPT
076600180104     C+               FETCH FIRST 1 ROW ONLY)
076700180104     C/END-EXEC
076800170626     C
076900170626     C*
077000170627     C                   if        %subst(wBUFFER:1:1) = *on
077100170628     C                   eval      DS_DCDP    = %subst(wBUFFER:2)
077200170627     C                   eval      oD_DPTISO2 = DS_DCDP.oD_DPTISO2
077300170627     C                   eval      oD_DSTR    = DS_DCDP.oD_DSTR
077400170627     C                   eval      oD_DPTIATA = DS_DCDP.oD_DPTIATA
077500170629     C                   eval      oD_GRPL    = DS_DCDP.oD_GRPL
077600170626     C                   else
077700170626     C                   eval      oD_DPTISO2 = oD_ISO2
077800170626     C                   endif
077900170626     C*
078000170626     C                   else
078100170628     C                   eval      wNotFoundCR2 = '1'
078200170626     C                   endif
078300170626     C
078400170626     C/EXEC SQL
078500170626     C+ close C_R2
078600170626     C/END-EXEC
078700170626     C
078800170626     C*
078900170628     C                   if        wNotFoundCR2 = *on
079000170626     C                   eval      OSID4ERR  = '1'
079100170626     C                   eval      OSID4ERRL = '96'
079200170626     C                   eval      OSID4ERRD = 'Errore in calcolo ' +
079300170626     C                             'instradamento R2 (DPCR210F)'
079400170626     C                   seton                                        70
079500170626     C                   endif
079600170626     C*
079700170626     C                   ENDSR
079800170626
079900170626
080000170626     C*------------------------------------------------------------------------*
080100170626     C* RTVCR3 - Esecuzione ricerca R3
080200170626     C*------------------------------------------------------------------------*
080300170626     C     RTVCR3        BEGSR
080400170626     C*
080500170626     C* Inizializzazioni
080600170626     C                   clear                   oD_OSORT
080700170626     C                   clear                   stringaSQL
080800170628     C                   clear                   wNotFoundCR3
080900170626     C*
081000170626     C* Compongo la stringa SQL
081100170626       stringaSQL =
081200170626       'SELECT CR3OSORT FROM DPCR310F ' +
081300170626       'WHERE CR3ATB=§ § and CR3VER='+%editc(sVERSION:'4')+' and ' +
081400170626       'CR3ISO2=§'+%trim(oD_ISO2)+'§ and ' +
081500170626       '(CR3DDPT=§ § or CR3DDPT=§'+oD_DPT+'§) and ' +
081600170626       '(CR3PTCB=§ § or ' +
081700170626       '§'+%trim(USID4CAD)+'§ between CR3PTCB and CR3PTCE) and ' +
081800170706       '(CR3NTWP=§ § or CR3NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
081900170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
082000170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
082100170626       ' (CR3ROUT=§ § and CR3ROUPB=§ §) or' +
082200170627       ' (CR3ROUT=§I§ and §'+oO_ICC+'§ between CR3ROUPB and CR3ROUPE) or'+
082300170627       ' (CR3ROUT=§C§ and CR3ROUPB=§'+oO_ISO2+'§) or ' +
082400170627       '(CR3ROUT=§D§ and §'+oO_DPT+'§ between CR3ROUPB and CR3ROUPE) or'+
082500170626       ' (CR3ROUT=§G§ and CR3ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
082600170627       ' (CR3ROUT=§B§ and CR3ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
082700170626       ') ' +
082800170626       'ORDER BY CR3PTY DESC ' +
082900170627       'FETCH FIRST 1 ROW ONLY ';
083000170627
083100170626     C*
083200170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
083300170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
083400170626     C
083500170626     C* Quindi eseguo stringa SQL così ottenuta
083600170626     C*
083700170626     C/EXEC SQL
083800170626     C+ prepare S_R3 from :stringaSQL
083900170626     C/END-EXEC
084000170626     C
084100170626     C/EXEC SQL
084200170626     C+ declare C_R3 asensitive cursor for S_R3
084300170626     C/END-EXEC
084400170626     C
084500170626     C/EXEC SQL
084600170626     C+ open C_R3
084700170626     C/END-EXEC
084800170626     C
084900170626     C/EXEC SQL
085000170626     C+ Fetch C_R3 into :oD_OSORT
085100170626     C/END-EXEC
085200170626     C*
085300170626     C                   if        sqlcod = *zeros
085400170626     C                   else
085500170628     C                   eval      wNotFoundCR3 = '1'
085600170626     C                   endif
085700170626     C
085800170626     C/EXEC SQL
085900170626     C+ close C_R3
086000170626     C/END-EXEC
086100170627     C
086200170626     C*
086300170626     C                   ENDSR
086400170627
086500170627
086600170627     C*------------------------------------------------------------------------*
086700170627     C* CHKALW - Verifica instradamenti consentiti ALLOW
086800170627     C*------------------------------------------------------------------------*
086900170627     C     CHKALW        BEGSR
087000170627     C*
087100170627     C* Inizializzazioni
087200170627     C                   clear                   oALLOWID
087300170627     C                   clear                   stringaSQL
087400170627     C                   clear                   wAllow
087500170627     C*
087600170627     C* Compongo la stringa SQL
087700180104      * stringaSQL =
087800180104      * 'SELECT CWSALWID FROM DPCWS10F ' +
087900180104      * 'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
088000180104      * '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
088100180104      * // '(CWSRULFT=§N§ and CWSRULFB=§'+oO_GRPL+'§) or ' +     //
088200180104      * '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
088300180104      * '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
088400180104      * '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
088500180104      * '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
088600180104      * '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and (' +
088700180104      * %editc(ISID4SORC:'X')+' between CWSRULSB and CWSRULSE or ' +
088800180104      * 'CWSRULSB=0) and (' +
088900180104      * '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
089000180104      * '(CWSRULTT=§C§ and CWSRULTB=§'+oD_DPTISO2+'§) or ' +
089100180104      * '(CWSRULTT=§N§ and CWSRULTB=§'+oD_NTWC+'§) or ' +
089200180104      * '(CWSRULTT=§B§ and CWSRULTB=§'+%editc(oD_BUCN:'X')+oD_DPTISO2+'§) or ' +
089300180104      * '(CWSRULTT=§G§ and CWSRULTB in (§'+%trim(oD_GRPL)+'§)) ) and (' +
089400180104      * '(§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWSZONTB and CWSZONTE or ' +
089500180104      * '(CWSZONTB=§ § and CWSZONTE=§ §))) ' +
089600180104      *
089700180104      * 'FETCH FIRST 1 ROW ONLY ';
089800180104      ************
089900180104       stringaSQL =
090000180104       'SELECT CWSALWID FROM DPCWS10F ' +
090100180104       'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
090200180104       '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
090300180104       // '(CWSRULFT=§N§ and CWSRULFB=§'+oO_GRPL+'§) or ' +     //
090400180104       '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
090500180104       '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
090600180104       '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
090700180104       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
090800180104       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and (' +
090900180104       %editc(ISID4SORC:'X')+' between CWSRULSB and CWSRULSE or ' +
091000180104       'CWSRULSB=0) and (' +
091100180104       '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
091200180104       '(CWSRULTT=§C§ and CWSRULTB=§'+oD_ISO2+'§) or ' +
091300180104       '(CWSRULTT=§N§ and CWSRULTB=§'+oD_NTWC+'§) or ' +
091400180104       '(CWSRULTT=§B§ and CWSRULTB=§'+%editc(oD_BUCN:'X')+oD_DPTISO2+'§) or ' +
091500180104       '(CWSRULTT=§G§ and CWSRULTB in (§'+%trim(oD_GRPL)+'§)) ) and (' +
091600180104       '(§'+oD_ISO2+%trim(USID4CAD)+'§ between CWSZONTB and CWSZONTE or ' +
091700180104       '(CWSZONTB=§ § and CWSZONTE=§ §))) ' +
091800180104
091900180104       'FETCH FIRST 1 ROW ONLY ';
092000180104
092100170627     C*
092200170627     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
092300170627     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
092400170627     C
092500170627     C* Quindi eseguo stringa SQL così ottenuta
092600170627     C*
092700170627     C/EXEC SQL
092800170627     C+ prepare S_WS from :stringaSQL
092900170627     C/END-EXEC
093000170627     C
093100170627     C/EXEC SQL
093200170627     C+ declare C_WS asensitive cursor for S_WS
093300170627     C/END-EXEC
093400170627     C
093500170627     C/EXEC SQL
093600170627     C+ open C_WS
093700170627     C/END-EXEC
093800170627     C
093900170627     C/EXEC SQL
094000170627     C+ Fetch C_WS into :oALLOWID
094100170627     C/END-EXEC
094200170627     C*
094300170627     C                   if        sqlcod = *zeros
094400170627     C                   eval      wAllow = '1'
094500170627     C                   endif
094600170627     C
094700170627     C/EXEC SQL
094800170627     C+ close C_WS
094900170627     C/END-EXEC
095000170627     C
095100170627     C                   if        wAllow = *on
095200170629     C*
095300170629     C* Se richiesto anche un ADDITIONAL SERVICE CODE => verifico se ALLOW
095400170629     C                   if        ISID4ASCO <> *blanks
095500170629     C                   exsr      CHKCWA
095600170629     C                   endif
095700170627     C                   else
095800170627     C                   eval      OSID4ERR  = '1'
095900170627     C                   eval      OSID4ERRL = '97'
096000170627     C                   eval      OSID4ERRD = 'Instradamento non ' +
096100170627     C                             'consentito. (DPCWS10F)'
096200170627     C                   seton                                        70
096300170627     C                   endif
096400170627     C*
096500170627     C                   ENDSR
096600170629
096700170629
096800170629     C*------------------------------------------------------------------------*
096900170629     C* CHKCWA - Verifica instradamenti consentiti ALLOW - ASCODE
097000170629     C*------------------------------------------------------------------------*
097100170629     C     CHKCWA        BEGSR
097200170629     C*
097300170629     C* Inizializzazioni
097400170629     C                   clear                   stringaSQL
097500170629     C                   clear                   wFound
097600170629     C*
097700170629     C* Compongo la stringa SQL
097800170629       stringaSQL =
097900170629       'SELECT §1§ FROM DPCWA10F ' +
098000170629       'WHERE CWAATB=§ § and CWAVER='+%editc(sVERSION:'4')+' and ' +
098100170629       'CWAALWID='+%editc(oALLOWID:'4')+' and (§' +
098200170629       ISID4ASCO+'§ between CWARULSB and CWARULSE or CWARULSB=§ §) and (' +
098300170629       '§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWAZONTB and CWAZONTE)) ' +
098400170629
098500170629       'FETCH FIRST 1 ROW ONLY ';
098600170629
098700170629     C*
098800170629     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
098900170629     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
099000170629     C
099100170629     C* Quindi eseguo stringa SQL così ottenuta
099200170629     C*
099300170629     C/EXEC SQL
099400170629     C+ prepare S_WA from :stringaSQL
099500170629     C/END-EXEC
099600170629     C
099700170629     C/EXEC SQL
099800170629     C+ declare C_WA asensitive cursor for S_WA
099900170629     C/END-EXEC
100000170629     C
100100170629     C/EXEC SQL
100200170629     C+ open C_WA
100300170629     C/END-EXEC
100400170629     C
100500170629     C/EXEC SQL
100600170629     C+ Fetch C_WS into :wFound
100700170629     C/END-EXEC
100800170629     C*
100900170629     C                   if        wFound = *on
101000170629     C                   else
101100170629     C                   eval      OSID4ERR  = '1'
101200170629     C                   eval      OSID4ERRL = '97'
101300170629     C                   eval      OSID4ERRD = 'Instradamento non ' +
101400170629     C                             'consentito. (DPCWA10F)'
101500170629     C                   seton                                        70
101600170629     C                   endif
101700170629     C
101800170629     C/EXEC SQL
101900170629     C+ close C_WS
102000170629     C/END-EXEC
102100170629     C
102200170629     C*
102300170629     C                   ENDSR
102400170626
102500170626
102600170626     C*------------------------------------------------------------------------*
102700170626     C* RTVCR4 - Esecuzione ricerca R4
102800170626     C*------------------------------------------------------------------------*
102900170626     C     RTVCR4        BEGSR
103000170626     C*
103100170626     C* Inizializzazioni
103200170626     C                   clear                   oD_CSORT
103300170626     C                   clear                   stringaSQL
103400170628     C                   clear                   wNotFoundCR4
103500170626     C*
103600170626     C* Compongo la stringa SQL
103700170626       stringaSQL =
103800170627       'SELECT CR4CSORT FROM DPCR410F ' +
103900170626       'WHERE CR4ATB=§ § and CR4VER='+%editc(sVERSION:'4')+' and ' +
104000170626       'CR4ISO2=§'+%trim(oD_ISO2)+'§ and ' +
104100170626       '(CR4DDPT=§ § or CR4DDPT=§'+oD_DPT+'§) and ' +
104200170706       '(CR4NTWP=§ § or CR4NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
104300170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
104400170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
104500170626       ' (CR4ROUT=§ § and CR4ROUPB=§ §) or' +
104600170627       ' (CR4ROUT=§I§ and §'+oO_ICC+'§ between CR4ROUPB and CR4ROUPE) or'+
104700170627       ' (CR4ROUT=§C§ and CR4ROUPB=§'+oO_ISO2+'§) or ' +
104800170627       '(CR4ROUT=§D§ and §'+oO_DPT+'§ between CR4ROUPB and CR4ROUPE) or'+
104900170626       ' (CR4ROUT=§G§ and CR4ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
105000170629       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
105100170627       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
105200170626       ') ' +
105300170626       'ORDER BY CR4PTY DESC ' +
105400170627       'FETCH FIRST 1 ROW ONLY ';
105500170627
105600170626     C*
105700170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
105800170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
105900170626     C
106000170626     C* Quindi eseguo stringa SQL così ottenuta
106100170626     C*
106200170626     C/EXEC SQL
106300170626     C+ prepare S_R4 from :stringaSQL
106400170626     C/END-EXEC
106500170626     C
106600170626     C/EXEC SQL
106700170626     C+ declare C_R4 asensitive cursor for S_R4
106800170626     C/END-EXEC
106900170626     C
107000170626     C/EXEC SQL
107100170626     C+ open C_R4
107200170626     C/END-EXEC
107300170626     C
107400170626     C/EXEC SQL
107500170626     C+ Fetch C_R4 into :oD_CSORT
107600170626     C/END-EXEC
107700170626     C*
107800170626     C                   if        sqlcod = *zeros
107900170626     C                   else
108000170628     C                   eval      wNotFoundCR4 = '1'
108100170626     C                   endif
108200170626     C
108300170626     C/EXEC SQL
108400170626     C+ close C_R4
108500170626     C/END-EXEC
108600170626     C
108700170626     C*
108800170626     C                   ENDSR
108900170626
109000170626
109100170626     C*------------------------------------------------------------------------*
109200170626     C* RTVCR5 - Esecuzione ricerca R5
109300170626     C*------------------------------------------------------------------------*
109400170626     C     RTVCR5        BEGSR
109500170626     C*
109600170626     C* Inizializzazioni
109700170626     C                   clear                   oD_DSORT
109800170626     C                   clear                   stringaSQL
109900170628     C                   clear                   wNotFoundCR5
110000170626     C*
110100170626     C* Compongo la stringa SQL
110200170626       stringaSQL =
110300170627       'SELECT CR5DSORT FROM DPCR510F ' +
110400170626       'WHERE CR5ATB=§ § and CR5VER='+%editc(sVERSION:'4')+' and ' +
110500170724       'CR5ISO2=§'+%trim(oD_ISO2)+'§ and ' +
110600170627       'CR5DDPT=§'+oD_DPT+'§ and ' +
110700170626       '(CR5PTCB=§ § or ' +
110800170626       '§'+%trim(USID4CAD)+'§ between CR5PTCB and CR5PTCE) and ' +
110900170706       '(CR5NTWP=§ § or CR5NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
111000170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
111100170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
111200170626       ' (CR5ROUT=§ § and CR5ROUPB=§ §) or' +
111300170627       ' (CR5ROUT=§I§ and §'+oO_ICC+'§ between CR5ROUPB and CR5ROUPE) or'+
111400170627       ' (CR5ROUT=§C§ and CR5ROUPB=§'+oO_ISO2+'§) or ' +
111500170627       '(CR5ROUT=§D§ and §'+oO_DPT+'§ between CR5ROUPB and CR5ROUPE) or'+
111600170626       ' (CR5ROUT=§G§ and CR5ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
111700170629       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
111800170627       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
111900170626       ') ' +
112000170626       'ORDER BY CR5PTY DESC ' +
112100170627       'FETCH FIRST 1 ROW ONLY ';
112200170627
112300170626     C*
112400170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
112500170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
112600170626     C
112700170626     C* Quindi eseguo stringa SQL così ottenuta
112800170626     C*
112900170626     C/EXEC SQL
113000170626     C+ prepare S_R5 from :stringaSQL
113100170626     C/END-EXEC
113200170626     C
113300170626     C/EXEC SQL
113400170626     C+ declare C_R5 asensitive cursor for S_R5
113500170626     C/END-EXEC
113600170626     C
113700170626     C/EXEC SQL
113800170626     C+ open C_R5
113900170626     C/END-EXEC
114000170626     C
114100170626     C/EXEC SQL
114200170626     C+ Fetch C_R5 into :oD_DSORT
114300170626     C/END-EXEC
114400170626     C*
114500170626     C                   if        sqlcod = *zeros
114600170626     C                   else
114700170628     C                   eval      wNotFoundCR5 = '1'
114800170626     C                   endif
114900170626     C
115000170626     C/EXEC SQL
115100170626     C+ close C_R5
115200170626     C/END-EXEC
115300170626     C
115400170626     C*
115500170626     C                   ENDSR
115600170627
115700170627
115800170627     C*------------------------------------------------------------------------*
115900170627     C* EXEOUT - Valorizzo paramteri di output
116000170627     C*------------------------------------------------------------------------*
116100170627     C     EXEOUT        BEGSR
116200170627     C*
116300170627     C                   eval      OSID4VER   = sVERSION
116400170627     C                   eval      OSID4VERD  = oVER_DPD
116500170627     C                   eval      OSID4VDDE  = CVEDDE
116600170627     C                   eval      OSID4DBUCN = oD_BUCN
116700170627     C                   eval      OSID4DBUCA = oD_BUCA
116800170627     C                   eval      OSID4DNTWC = oD_NTWC
116900170627     C                   eval      OSID4DDPTC = oD_DPTISO2
117000170627     C                   eval      OSID4DDPT  = oD_DPT
117100170627     C                   eval      OSID4DSTR  = oD_DSTR
117200170627     C                   eval      OSID4SSORT = oD_SSORT
117300170627     C                   eval      OSID4DPTNC = oD_PTNC
117400170627     C                   eval      OSID4OSORT = oD_OSORT
117500170627     C                   eval      OSID4CSORT = oD_CSORT
117600170627     C                   eval      OSID4DSORT = oD_DSORT
117700170627     C                   eval      OSID4BCID  = oVER_BCID
117800170627     C                   eval      OSID4SRVX  = oSRV_TXT
117900170627     C                   eval      OSID4SRVM  = oSRV_MRK
118000170627     C                   eval      OSID4ODPT  = oO_DPT
118100170627     C                   eval      OSID4ODPTC = oO_ISO2
118200170627     C                   eval      OSID4OBUCN = oO_BUCN
118300170627     C                   eval      OSID4OGRPL = oO_GRPL
118400170627     C                   eval      OSID4OICC  = oO_ICC
118500170627     C                   eval      OSID4ALWID = oALLOWID
118600170627     C                   eval      OSID4IATA  = oD_DPTIATA
118700170629     C                   evalr     OSID4CADP  = %trim(USID4CAD)
118800170629     C                   eval      OSID4CADP = %scanrpl(' ':'0':OSID4CADP)
118900170627     C*
119000170627     C* Sancisco esito a OK
119100170627     C                   movel     *blanks       OSID4ERR
119200170627     C*
119300170627     C                   ENDSR
119400170626
119500170626
119600170626     C*------------------------------------------------------------------------*
119700170626     C* CARTAB - Caricamemto tabelle si procedura
119800170626     C*------------------------------------------------------------------------*
119900170626     C     CARTAB        BEGSR
120000170626     C*
120100170626     C* Reperisco i valori tabellati di procedura
120200170626     C                   eval      tblKUT = 1
120300170626     C                   eval      tblCOD = '3I'
120400170626     C                   eval      tblKEY ='DPD'
120500170626     C     KEYtab00_C    chain     tabel00f
120600170626     C                   if        %found(tabel00f)
120700170626     C                   eval      DS3IDP = tblUNI
120800170627     C                   eval      gBU   = §3IBUCN
120900170627     C                   eval      gISO2 = §3IISO2
121000170626     C                   else
121100170626     C                   eval      OSID4ERR  = '1'
121200170627     C                   eval      OSID4ERRL = '80'
121300170627     C                   eval      OSID4ERRD = 'Tabella 3I valore §3IBUC non ' +
121400170626     C                             'trovato'
121500170626     C                   seton                                        70
121600170626     C                   endif
121700170626     C*
121800170626     C                   ENDSR
121900170620
122000170620
122100170620     C*--------------------------------------------------------------------------------------------*
122200060515     C* *inzsr - operazioni iniziali
122300060515     C*--------------------------------------------------------------------------------------------*
122400000000     C     *inzsr        BEGSR
122500060515     C*
122600060515     C* Ricevimento parametri
122700060515     C     *ENTRY        PLIST
122800170627     C                   PARM                    TISID4DS
122900060510     C*
123000060510     C* CALCOLA LA DATA CORRENTE
123100170620     C                   Z-ADD     *zeros        datcor            8 0
123200170620     C                   EVAL      datcor = %dec(%date() : *iso)
123300170626     C*
123400170626     C* CARICAMENTO TABELLE DI PROCEDURA
123500170626     C                   EXSR      CARTAB
123600170626     C*
123700170626     C* TABEL00F - Completa
123800170626     C     KEYtab00_C    KLIST
123900170626     C                   KFLD                    tblKUT
124000170626     C                   KFLD                    tblCOD
124100170626     C                   KFLD                    tblKEY
124200170622     C*
124300170622     C* DPCCC14I - Completa
124400170622     C     KEYccc14_C    KLIST
124500170622     C                   KFLD                    cccATB
124600170622     C                   KFLD                    cccVER
124700170622     C                   KFLD                    cccBRT
124800060515     C*
124900060515     C                   ENDSR
