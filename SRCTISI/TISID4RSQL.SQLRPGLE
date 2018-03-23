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
007300180110     D SkLNP           s              3S 0 dim(500)inz
007400180110     D SkDPT           s              7A   dim(500)inz
007500170622     D jLNP            s              3S 0 inz
007600170622     D sjLNP           s                   like(jLNP) inz
007700180110     D SkNZD           s              3A   dim(500)inz
007800180110     D SkISO2          s              2A   dim(500)inz
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
008900180110     D primo           s              2A   inz
009000180110     D*
009100170627     D wBUFFER         s            256A   inz
009200170629     D DS_CSO          ds                  qualified inz
009300170627     D   oSRV_TXT                          like(oSRV_TXT)
009400170627     D   oSRV_MRK                          like(oSRV_MRK)
009500170626     D*
009600170627     D DS_OCDP         ds                  qualified inz
009700170627     D   oO_ISO2                           like(oO_ISO2)
009800170627     D   oO_BUCN                           like(oO_BUCN)
009900170627     D   oO_GRPL                           like(oO_GRPL)
010000170626     D*
010100170627     D DS_DCDP         ds                  qualified inz
010200170627     D   oD_DPTISO2                        like(oD_DPTISO2)
010300170627     D   oD_DSTR                           like(oD_DSTR)
010400170627     D   oD_DPTIATA                        like(oD_DPTIATA)
010500170629     D   oD_GRPL                           like(oD_GRPL)
010600170620
010700170620
010800170620     C*--------------------------------------------------------------------------------------------*
010900170620     C* Main lines
011000170620     C*--------------------------------------------------------------------------------------------*
011100150518     C*
011200150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
011300150518     C
011400150518     C/EXEC SQL
011500150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
011600150518     C/END-EXEC
011700170620     C*
011800170620     C* Avvio il monitoring del intero flusso
011900170620     C                   monitor
012000170622     C*
012100170622     C* Resetto indicatore d errore
012200170622     C                   setoff                                       70
012300170620     C*
012400170620     C* Inizializzazioni
012500170622     C                   exsr      exeINZ
012600170622     C*
012700170622     C* Step 01 - Verifica parametri di input
012800170626     C  N70              exsr      chkPAR
012900170622     C*
013000170622     C* Step 02 - Reperimento dati Versione
013100170622     C  N70              exsr      rtvVER
013200170622     C*
013300170626     C* Step 03 - Reperimento Depot DPD (partenza)
013400170626     C  N70              exsr      rtvDPT
013500170622     C*
013600170626     C* Step 04 - Reperimento DESTINATION Nazione ISO 2
013700170626     C  N70              exsr      rtvDISO2
013800170622     C*
013900170622     C* Step 05 - Verifica del Pattern CAP
014000170622     C  N70              exsr      chkPATT
014100170622     C*
014200170622     C* Step 06 - Verifica esistenza CAP in DPD
014300170622     C  N70              exsr      chkPTC
014400170622     C*
014500170622     C* Step 07 - Reperimento dati SOCODE
014600170622     C  N70              exsr      rtvSORC
014700170622     C*
014800170622     C* Step 08 - Reperimento dati ORIGIN DEPOT
014900170626     C  N70              exsr      rtvODPT
015000170626     C*
015100170626     C* Step 09 - Reperimento dati INJECTION CODE
015200170626     C  N70              exsr      rtvCIC
015300170626     C*
015400170626     C* Step 10 - Esecuzione ricerca R1
015500170626     C  N70              exsr      rtvCR1
015600170626     C*
015700170626     C* Step 11 - Esecuzione ricerca R2
015800170626     C  N70              exsr      rtvCR2
015900170626     C*
016000170627     C* Step 12 - Verifica instradamenti consentiti ALLOW
016100170627     C  N70              exsr      chkALW
016200170626     C*
016300170626     C* Step 13 - Esecuzione ricerca R3
016400170626     C  N70              exsr      rtvCR3
016500170626     C*
016600170626     C* Step 14 - Esecuzione ricerca R4
016700170626     C  N70              exsr      rtvCR4
016800170626     C*
016900170626     C* Step 15 - Esecuzione ricerca R5
017000170626     C  N70              exsr      rtvCR5
017100170620     C*
017200170627     C* Step 16 - Valorizzo paramteri di output
017300170627     C  N70              exsr      exeOUT
017400170620     C*
017500170620     C* Gestisco eventuale errore
017600170620     C                   on-error
017700170620     C                   dump(A)
017800170627     C                   eval      OSID4ERR = 'E'
017900170620     C*
018000170620     C* Arresto il monitoring
018100170620     C                   endmon
018200170620     C*
018300180131     C*********          seton                                        rt
018400060515     C*
018500180131     C                   seton                                        LR
018600170627
018700170627
018800170622     C*------------------------------------------------------------------------*
018900170622     C* EXEINZ - Inizializzazioni
019000170622     C*------------------------------------------------------------------------*
019100170627     C     EXEINZ        BEGSR
019200170622     C*
019300170627     C* Variabili di work
019400170627     C                   clear                   oD_BUCN
019500170627     C                   clear                   oD_BUCA
019600170627     C                   clear                   oD_NTWC
019700170628     C                   clear                   oD_ISON
019800170627     C                   clear                   oD_DPTISO2
019900170627     C                   clear                   oD_DPT
020000170627     C                   clear                   oD_DSTR
020100170627     C                   clear                   oD_SSORT
020200170627     C                   clear                   oD_PTNC
020300170627     C                   clear                   oD_OSORT
020400170627     C                   clear                   oD_CSORT
020500170627     C                   clear                   oD_DSORT
020600170627     C                   clear                   oSRV_TXT
020700170627     C                   clear                   oSRV_MRK
020800170627     C                   clear                   oO_DPT
020900170627     C                   clear                   oO_ISO2
021000170627     C                   clear                   oO_BUCN
021100170627     C                   clear                   oO_GRPL
021200170627     C                   clear                   oO_ICC
021300170627     C                   clear                   oALLOWID
021400170627     C                   clear                   oD_DPTIATA
021500170627     C*
021600170627     C* Paramteri di output
021700170629     C                   clear                   USID4CAD
021800170627     C                   clear                   OSID4VER
021900170627     C                   clear                   OSID4VERD
022000170627     C                   clear                   OSID4VDDE
022100170627     C                   clear                   OSID4DBUCN
022200170627     C                   clear                   OSID4DBUCA
022300170627     C                   clear                   OSID4DNTWC
022400170627     C                   clear                   OSID4DDPTC
022500170627     C                   clear                   OSID4DDPT
022600170627     C                   clear                   OSID4DSTR
022700170627     C                   clear                   OSID4SSORT
022800170627     C                   clear                   OSID4DPTNC
022900170627     C                   clear                   OSID4OSORT
023000170627     C                   clear                   OSID4CSORT
023100170627     C                   clear                   OSID4DSORT
023200170627     C                   clear                   OSID4BCID
023300170627     C                   clear                   OSID4SRVX
023400170627     C                   clear                   OSID4SRVM
023500170627     C                   clear                   OSID4ODPT
023600170627     C                   clear                   OSID4ODPTC
023700170627     C                   clear                   OSID4OBUCN
023800170627     C                   clear                   OSID4OGRPL
023900170627     C                   clear                   OSID4OICC
024000170627     C                   clear                   OSID4ALWID
024100170629     C                   clear                   OSID4IATA
024200170629     C                   clear                   OSID4CADP
024300170627     C                   clear                   OSID4ERRL
024400170627     C                   clear                   OSID4ERRD
024500170622     C*
024600170627     C* Inizializzo l'esito
024700170627     C                   movel     '0'           OSID4ERR
024800170627     C*
024900170622     C                   ENDSR
025000170622
025100170622
025200170622     C*------------------------------------------------------------------------*
025300170622     C* CHKPAR - Verifica parametri di input
025400170622     C*------------------------------------------------------------------------*
025500170622     C     CHKPAR        BEGSR
025600170622     C*
025700170622     C                   setoff                                       40
025800170622     C*
025900170622     C* Data di riferimento
026000170622     C                   if        ISID4DRI = *zeros
026100170626     C                   eval      ISID4DRI = datcor
026200170622     C                   endif
026300170622     C*
026400170622     C* Linea di Partenza
026500170622     C                   if        ISID4LNP = *zeros
026600170622     C                   seton                                        40
026700170622     C                   endif
026800170622     C*
026900170622     C* CAP destinatario
027000170622     C                   if        ISID4CAD = *blanks
027100170622     C                   seton                                        40
027200170629     C                   else
027300170629     C                   eval      USID4CAD = ISID4CAD
027400170622     C                   endif
027500170629     C*
027600170629     C* Forzatura su CAP destinatario per NAZIONE IRLANDA
027700170629     C                   if        ISID4NZD = 'IRL'
027800180108     C                             and ISID4CAD <>'2'
027900170629     C                   eval      USID4CAD = '1'
028000170629     C                   endif
028100170622     C*
028200170622     C* Servizio DPD
028300170622     C                   if        ISID4SORC = *zeros
028400170622     C                   seton                                        40
028500170622     C                   endif
028600170622     C*
028700170622     C* Se nn ok => imposto subito l'errore generale d procedura
028800170622     C                   if        *in40
028900170622     C                   eval      OSID4ERR  = '1'
029000170622     C                   eval      OSID4ERRL = '92'
029100170622     C                   eval      OSID4ERRD = 'Errore: parametri obbligatori '+
029200170622     C                             'non valorizzati'
029300170622     C                   seton                                        70
029400170622     C                   endif
029500170622     C*
029600170622     C                   ENDSR
029700170622
029800170622
029900170622     C*------------------------------------------------------------------------*
030000170622     C* RTVVER - Reperimento VERSIONE richiesta
030100170622     C*------------------------------------------------------------------------*
030200170622     C     RTVVER        BEGSR
030300170622     C*
030400170622     C* Reperisco VERSIONE solo se non ancora reperita o data riferimento cambiata
030500170626     C                   if        sVERSION  = *zeros    OR
030600170626     C                             ISID4DRI <> sDAT_RIF
030700170626     C*
030800170626     C                   eval      sDAT_RIF = ISID4DRI
030900170622     C*
031000170622     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
031100170627     C                   call      'TISID1R'
031200170626     C                   parm                    sDAT_RIF
031300170626     C                   parm                    sVERSION
031400170627     C                   parm                    oVER_DPD
031500170627     C                   parm                    CVEDDE
031600170627     C                   parm                    CVEDSC
031700170626     C                   parm                    oVER_BCID
031800170622     C*
031900170622     C* Se reperita versione valida
032000170626     C                   if        sVERSION  > *zeros
032100170622     C                   else
032200170622     C*
032300170622     C* Se non ok => imposto subito l'errore generale d procedura
032400170622     C                   eval      OSID4ERR  = '1'
032500170622     C                   eval      OSID4ERRL = '90'
032600170622     C                   eval      OSID4ERRD = 'Errore in reperimento versione'+
032700170622     C                             ' (DPCVE10F)'
032800170622     C                   seton                                        70
032900170622     C                   endif
033000170622     C                   endif
033100170622     C*
033200170622     C                   ENDSR
033300170622
033400170622
033500170622     C*------------------------------------------------------------------------*
033600170626     C* RTVDPT - Reperimento DEPOT DPD (partenza)
033700170622     C*------------------------------------------------------------------------*
033800170626     C     RTVDPT        BEGSR
033900170622     C*
034000170626     C                   clear                   oO_DPT
034100170622     C*
034200170622     C* Cerco abbinamento Filiale BRT <=> Depot DPD
034300170627     C                   z-add     1             jLNP
034400170622     C     ISID4LNP      lookup    SkLNP(jLNP)                            55
034500170622     C                   if        %found
034600170626     C                   eval      oO_DPT = SkDPT(jLNP)
034700170622     C                   else
034800170622     C*
034900170622     C     ISID4LNP      chain     azorg01l
035000170622     C                   if        %found(azorg01l)
035100170622     C                   eval      OG143 = ORGDE3
035200170622     C                   add       1             sjLNP
035300170626     C                   eval      SkLNP(sjLNP) = ISID4LNP
035400170627     C                   eval      SkDPT(sjLNP) = %editc(gBU:'X')+§OGDP1
035500170626     C                   eval      oO_DPT = SkDPT(sjLNP)
035600170622     C                   endif
035700170622     C*
035800170626     C                   if        oO_DPT = *blanks
035900170622     C                   eval      OSID4ERR  = '1'
036000170622     C                   eval      OSID4ERRL = '91'
036100170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
036200170622     C                             'partenza DPD (AZORG01L)'
036300170622     C                   seton                                        70
036400170622     C                   endif
036500170622     C                   endif
036600170622     C*
036700170622     C                   ENDSR
036800170622
036900170622
037000170622     C*------------------------------------------------------------------------*
037100170626     C* RTVDISO2 - Reperimento DESTINATION CODICE NAZIONE ISO 2
037200170622     C*------------------------------------------------------------------------*
037300170626     C     RTVDISO2      BEGSR
037400170622     C*
037500170622     C                   clear                   oD_ISO2
037600170622     C*
037700170622     C* Cerco abbinamento Cod Nazione BRT <=> Codice Nazione ISO2
037800170627     C                   z-add     1             jNZD
037900170622     C     ISID4NZD      lookup    SkNZD(jNZD)                            55
038000180110     C                   if        %found AND
038100180110     C                             (ISID4NZD <> *blanks or
038200180110     C                              ISID4NZD  = *blanks and primo ='NO')
038300170630     C                   eval      oD_ISO2 = SkISO2(jNZD)
038400170622     C                   else
038500170622     C*
038600170622     C                   eval      cccATB = *blanks
038700170626     C                   eval      cccVER = sVERSION
038800170622     C                   eval      cccBRT = ISID4NZD
038900170622     C     KEYccc14_C    chain     dpccc14i
039000170622     C                   if        %found(dpccc14i)
039100180110      *
039200180110      *   se ha caricato la prima volta il blank come 'IT'
039300180110     c                   if        primo = *blank and ISID4NZD = *blanks
039400180110     c                   eval      primo = 'NO'
039500180110     C                   endif
039600180110      *
039700170622     C                   add       1             sjNZD
039800170622     C                   eval      SkNZD(sjNZD)  = ISID4NZD
039900170622     C                   eval      SkISO2(sjNZD) = cccISO2
040000170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
040100170622     C                   endif
040200170622     C*
040300170622     C                   if        oD_ISO2 = *blanks
040400170622     C                   eval      OSID4ERR  = '1'
040500170622     C                   eval      OSID4ERRL = '93'
040600170622     C                   eval      OSID4ERRD = 'Errore in reperimento nazione '+
040700170622     C                             'destino (DPCCC10F)'
040800170622     C                   seton                                        70
040900170622     C                   endif
041000170622     C                   endif
041100170622     C*
041200170622     C                   ENDSR
041300170620
041400170620
041500170620     C*------------------------------------------------------------------------*
041600170620     C* CHKPATT - Verifica del Pattern CAP
041700170620     C*------------------------------------------------------------------------*
041800170620     C     CHKPATT       BEGSR
041900170622     C*
042000170622     C* Inizializzazioni
042100170622     C                   clear                   TISID2DS
042200170622     C*
042300170622     C* Valorizzazioni
042400170627     C                   eval      ISID2VER   = sVERSION
042500170627     C                   eval      ISID2NISO2 = oD_ISO2
042600170629     C                   eval      ISID2PTC   = USID4CAD
042700170622     C                   call      'TISID2R'
042800170622     C                   parm                    TISID2DS
042900170622     C*
043000170622     C                   if        OSID2ESI  = 'V'
043100170622     C                   eval      USID4CAD = OSID2PTC
043200170622     C                   else
043300170622     C                   eval      OSID4ERR  = '1'
043400170627     C                   eval      OSID4ERRL = '99'
043500170627     C                   eval      OSID4ERRD = 'Errore - CAP errato ' +
043600170622     C                             '(Pattern DPCCC10F)'
043700170622     C                   seton                                        70
043800170622     C                   endif
043900170620     C*
044000170620     C                   ENDSR
044100170622
044200170622
044300170622     C*------------------------------------------------------------------------*
044400170622     C* CHKPTC - Verifica esistenza CAP in DPD
044500170622     C*------------------------------------------------------------------------*
044600170622     C     CHKPTC        BEGSR
044700170622     C*
044800170622     C* Inizializzazioni
044900170622     C                   movel     *blanks       wFound
045000170629     C*
045100170629     C* Se necessaria validazione del CAP rispetto alla NAZIONE
045200170629     C                   if        OSID2PVAL = 'VB'                             * blocked on failure
045300170629     C
045400170622     C/EXEC SQL
045500170622     C+ SET :wFound = (SELECT '1' FROM DPCPC10F WHERE CPCATB = ' ' AND
045600170626     C+                CPCVER = :sVERSION AND CPCISO2 = :oD_ISO2
045700170626     C+                AND :USID4CAD BETWEEN CPCPTCB AND CPCPTCE
045800170626     C+                FETCH FIRST 1 ROW ONLY)
045900170622     C/END-EXEC
046000170622     C
046100170622     C*
046200170622     C                   if        wFound = *on
046300170622     C                   else
046400170622     C                   eval      OSID4ERR  = '1'
046500170630     C                   eval      OSID4ERRL = '96'
046600170627     C                   eval      OSID4ERRD = 'Errore - CAP non trovato ' +
046700170622     C                             '(DPCPC10F)'
046800170622     C                   seton                                        70
046900170622     C                   endif
047000170629     C*
047100170629     C                   endif
047200170622     C*
047300170622     C                   ENDSR
047400170622
047500170622
047600170622     C*------------------------------------------------------------------------*
047700170622     C* RTVSORC - Reperimento dati SOCODE
047800170622     C*------------------------------------------------------------------------*
047900170622     C     RTVSORC       BEGSR
048000170622     C*
048100170622     C* Inizializzazioni
048200170627     C                   clear                   DS_CSO
048300170627     C                   clear                   wBUFFER
048400170622     C
048500170622     C/EXEC SQL
048600170629     C+ SET :wBUFFER = (SELECT '1' CONCAT CSOSTXT CONCAT CSOSMRK FROM DPCSO10F
048700170627     C+              WHERE CSOATB = ' ' AND CSOVER = :sVERSION AND
048800170627     C+              CSOSORC = :ISID4SORC
048900170627     C+              FETCH FIRST 1 ROW ONLY)
049000170622     C/END-EXEC
049100170622     C
049200170622     C*
049300170629     C                   if        %subst(wBUFFER:1:1) = *on
049400170628     C                   eval      DS_CSO   = %subst(wBUFFER:2)
049500170627     C                   eval      oSRV_TXT = DS_CSO.oSRV_TXT
049600170627     C                   eval      oSRV_MRK = DS_CSO.oSRV_MRK
049700170622     C                   else
049800170622     C                   eval      OSID4ERR  = '1'
049900170627     C                   eval      OSID4ERRL = '95'
050000170627     C                   eval      OSID4ERRD = 'Errore in reperimento servizio'+
050100170622     C                             ' (DPCSC10F)'
050200170622     C                   seton                                        70
050300170622     C                   endif
050400170622     C*
050500170622     C                   ENDSR
050600170622
050700170622
050800170622     C*------------------------------------------------------------------------*
050900170626     C* RTVODPT - Reperimento dati ORIGIN DEPOT
051000170622     C*------------------------------------------------------------------------*
051100170626     C     RTVODPT       BEGSR
051200170622     C*
051300170622     C* Inizializzazioni
051400170627     C                   clear                   DS_OCDP
051500170627     C                   clear                   wBUFFER
051600170622     C
051700170622     C/EXEC SQL
051800170627     C+ SET :wBUFFER  = (SELECT
051900170627     C+               '1' CONCAT CDPISO2 CONCAT digits(CDPBUCN) CONCAT CDPGRPID
052000170627     C+               FROM DPCDP10F
052100170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
052200170627     C+               CDPDPTN = :oO_DPT
052300170627     C+               FETCH FIRST 1 ROW ONLY)
052400170622     C/END-EXEC
052500170622     C
052600170622     C*
052700170627     C                   if        %subst(wBUFFER:1:1) = *on
052800170628     C                   eval      DS_OCDP = %subst(wBUFFER:2)
052900170627     C                   eval      oO_ISO2 = DS_OCDP.oO_ISO2
053000170627     C                   eval      oO_BUCN = DS_OCDP.oO_BUCN
053100170627     C                   eval      oO_GRPL = DS_OCDP.oO_GRPL
053200170622     C                   else
053300170622     C                   eval      OSID4ERR  = '1'
053400170626     C                   eval      OSID4ERRL = '91'
053500170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
053600170622     C                             'partenza DPD (DPCDP10F)'
053700170622     C                   seton                                        70
053800170622     C                   endif
053900170622     C*
054000170622     C                   ENDSR
054100170626
054200170626
054300170626     C*------------------------------------------------------------------------*
054400170626     C* RTVCIC - Reperimento dati INJECTION CODE
054500170626     C*------------------------------------------------------------------------*
054600170626     C     RTVCIC        BEGSR
054700170626     C*
054800170626     C* Inizializzazioni
054900170627     C                   clear                   wBUFFER
055000170626     C*
055100170626     C* Se indicato uns specifico SUBCODE in input
055200170626     C                   if        ISID4SUBC <> *blanks
055300170627     C                   eval      oO_ICC = oO_DPT + ISID4SUBC
055400170626     C
055500170626     C/EXEC SQL
055600170627     C+ SET :wBUFFER  = (SELECT '1' CONCAT CICICC
055700170627     C+               FROM DPCIC10F
055800170627     C+               WHERE CICATB = ' ' AND CICVER = :sVERSION AND
055900170627     C+               CICICC = :oO_ICC
056000170627     C+               FETCH FIRST 1 ROW ONLY)
056100170626     C/END-EXEC
056200170626     C
056300170626     C*
056400170627     C                   if        %subst(wBUFFER:1:1) = *on
056500170626     C                   else
056600170626     C                   eval      OSID4ERR  = '1'
056700170626     C                   eval      OSID4ERRL = '94'
056800170626     C                   eval      OSID4ERRD = 'Errore in reperimento ' +
056900170626     C                             'INJECTION CODE (DPCIC10F)'
057000170626     C                   seton                                        70
057100170626     C                   endif
057200170626     C*
057300170626     C                   endif
057400170626     C*
057500170626     C                   ENDSR
057600170626
057700170626
057800170626     C*------------------------------------------------------------------------*
057900170626     C* RTVCR1 - Esecuzione ricerca R1
058000170626     C*------------------------------------------------------------------------*
058100170626     C     RTVCR1        BEGSR
058200170626     C*
058300170626     C* Inizializzazioni
058400170626     C                   clear                   oD_BUCN
058500170626     C                   clear                   oD_BUCA
058600170628     C                   clear                   oD_NTWC
058700170628     C                   clear                   oD_ISON
058800170626     C                   clear                   stringaSQL
058900170628     C                   clear                   wNotFoundCR1
059000170626     C*
059100170626     C* Compongo la stringa SQL
059200170626       stringaSQL =
059300170626       'SELECT CR1BUCN FROM DPCR110F ' +
059400170626       'WHERE CR1ATB=§ § and CR1VER='+%editc(sVERSION:'4')+' and ' +
059500170627       'CR1TRADS=§'+ISID4TRADS+'§ and ' +
059600170626       'CR1ISO2=§'+%trim(oD_ISO2)+'§ and (CR1PTCB = § § or ' +
059700170626       '§'+%trim(USID4CAD)+'§ between CR1PTCB and CR1PTCE) and ' +
059800170706       '(CR1NTWP=§ § or CR1NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
059900170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
060000170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
060100170626       ' (CR1ROUT=§ § and CR1ROUPB=§ §) or' +
060200170627       ' (CR1ROUT=§I§ and §'+oO_ICC+'§ between CR1ROUPB and CR1ROUPE) or'+
060300170627       ' (CR1ROUT=§C§ and CR1ROUPB=§'+oO_ISO2+'§) or ' +
060400170627       '(CR1ROUT=§D§ and §'+oO_DPT+'§ between CR1ROUPB and CR1ROUPE) or'+
060500170626       ' (CR1ROUT=§G§ and CR1ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
060600170627       ' (CR1ROUT=§B§ and CR1ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
060700170626       ') ' +
060800170626       'ORDER BY CR1PTY DESC ' +
060900170627       'FETCH FIRST 1 ROW ONLY ';
061000170627
061100170626     C*
061200170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
061300170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
061400170626     C
061500170626     C* Quindi eseguo stringa SQL così ottenuta
061600170626     C*
061700170626     C/EXEC SQL
061800170626     C+ prepare S_R1 from :stringaSQL
061900170626     C/END-EXEC
062000170626     C
062100170626     C/EXEC SQL
062200170626     C+ declare C_R1 asensitive cursor for S_R1
062300170626     C/END-EXEC
062400170626     C
062500170626     C/EXEC SQL
062600170626     C+ open C_R1
062700170626     C/END-EXEC
062800170626     C
062900170626     C/EXEC SQL
063000170626     C+ Fetch C_R1 into :oD_BUCN
063100170626     C/END-EXEC
063200170626     C*
063300170626     C                   if        sqlcod = *zeros
063400170626     C*
063500170626     C* Quindi aggancio i dati della DESTINATION BUSINESS UNIT
063600170626     C
063700170626     C/EXEC SQL
063800170626     C+ SET :oD_BUCA = (SELECT CBUBUCA FROM DPCBU10F
063900170626     C+                 WHERE CBUATB = ' ' AND CBUVER = :sVERSION AND
064000170627     C+                 CBUBUCN = :oD_BUCN
064100170627     C+                 FETCH FIRST 1 ROW ONLY)
064200170626     C/END-EXEC
064300170626     C                   if        sqlcod <> *zeros
064400170628     C                   eval      wNotFoundCR1 = '1'
064500170626     C                   endif
064600170626     C*
064700170626     C* Quindi aggancio i dati del DESTINATION NETWORK CODE
064800170626     C
064900170626     C/EXEC SQL
065000170626     C+ SET :oD_NTWC = (SELECT CNTNTWC FROM DPCNT10F
065100170626     C+                 WHERE CNTATB = ' ' AND CNTVER = :sVERSION AND
065200170626     C+                 CNTISO2 = :oD_ISO2 AND CNTBUCN = :oD_BUCN
065300170626     C+                 FETCH FIRST 1 ROW ONLY)
065400170626     C/END-EXEC
065500170628     C                   if        sqlcod = *zeros
065600170628     C                   else
065700170628     C*
065800170628     C* Quindi aggancio i dati della NAZIONE
065900170628     C
066000170628     C/EXEC SQL
066100170628     C+ SET :oD_ISON = (SELECT CCCISON FROM DPCCC10F
066200170628     C+                 WHERE CCCATB = ' ' AND CCCVER = :sVERSION AND
066300170628     C+                 CCCISO2 = :oD_ISO2
066400170628     C+                 FETCH FIRST 1 ROW ONLY)
066500170628     C/END-EXEC
066600170628     C                   if        sqlcod = *zeros
066700170628     C                   eval      oD_NTWC = %editc(oD_ISON:'X')
066800170628     C                   else
066900170628     C                   eval      wNotFoundCR1 = '1'
067000170626     C                   endif
067100170628     C                   endif
067200170626     C*
067300170626     C                   else
067400170628     C                   eval      wNotFoundCR1 = '1'
067500170626     C                   endif
067600170626     C
067700170626     C/EXEC SQL
067800170626     C+ close C_R1
067900170626     C/END-EXEC
068000170626     C
068100170626     C*
068200170628     C***                if        wNotFoundCR1 = *on
068300170627     C***                eval      OSID4ERR  = '1'
068400170627     C***                eval      OSID4ERRL = '96'
068500170627     C***                eval      OSID4ERRD = 'Errore in calcolo ' +
068600170627     C***                          'instradamento R1 (DPCR110F)'
068700170627     C***                seton                                        70
068800170627     C***                endif
068900170626     C*
069000170626     C                   ENDSR
069100170626
069200170626
069300170626     C*------------------------------------------------------------------------*
069400170626     C* RTVCR2 - Esecuzione ricerca R2
069500170626     C*------------------------------------------------------------------------*
069600170626     C     RTVCR2        BEGSR
069700170626     C*
069800170626     C* Inizializzazioni
069900170626     C                   clear                   oD_DPT
070000170626     C                   clear                   oD_DPTISO2
070100170626     C                   clear                   oD_DSTR
070200170626     C                   clear                   oD_SSORT
070300170626     C                   clear                   oD_PTNC
070400170627     C                   clear                   oD_DPTIATA
070500170626     C                   clear                   stringaSQL
070600170628     C                   clear                   wNotFoundCR2
070700170627     C                   clear                   DS_DCDP
070800170627     C                   clear                   wBUFFER
070900170626     C*
071000170626     C* Compongo la stringa SQL
071100170626       stringaSQL =
071200170626       'SELECT CR2DDPT, CR2SSORT, CR2PTNC FROM DPCR210F ' +
071300170626       'WHERE CR2ATB=§ § and CR2VER='+%editc(sVERSION:'4')+' and ' +
071400170626       'CR2TRADS=§'+%trim(ISID4TRADS)+'§ and ' +
071500170626       'CR2BUCN='+%editc(oD_BUCN:'X')+' and ' +
071600180212       'CR2ISO2=§'+%trim(oD_ISO2)+'§ and (CR2PTCB = § § or §' +
071700180212       %trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE and ' +
071701180212       'CR2ISO2<>§GB§ or §' +
071702180212       %trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE and ' +
071703180213       'CR2ISO2 =§GB§ and CHARACTER_LENGTH(trim(CR2PTCB))<= ' +
071705180213       'CHARACTER_LENGTH(§' + %trim(USID4CAD) + '§) and ' +
071706180213       'CHARACTER_LENGTH(trim(CR2PTCE))>= ' +
071707180213       'CHARACTER_LENGTH(§' + %trim(USID4CAD) + '§)) and ' +
071800170706       '(CR2NTWP=§ § or CR2NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
071900170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
072000170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
072100170626       ' (CR2ROUT=§ § and CR2ROUPB=§ §) or' +
072200170627       ' (CR2ROUT=§I§ and §'+oO_ICC+'§ between CR2ROUPB and CR2ROUPE) or'+
072300170627       ' (CR2ROUT=§C§ and CR2ROUPB=§'+oO_ISO2+'§) or ' +
072400170627       '(CR2ROUT=§D§ and §'+oO_DPT+'§ between CR2ROUPB and CR2ROUPE) or'+
072500170626       ' (CR2ROUT=§G§ and CR2ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
072600170627       ' (CR2ROUT=§B§ and CR2ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
072700170626       ') ' +
072800170626       'ORDER BY CR2PTY DESC ' +
072900170627       'FETCH FIRST 1 ROW ONLY ';
073000170627
073100170626     C*
073200170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
073300170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
073400170626     C
073500170626     C* Quindi eseguo stringa SQL così ottenuta
073600170626     C*
073700170626     C/EXEC SQL
073800170626     C+ prepare S_R2 from :stringaSQL
073900170626     C/END-EXEC
074000170626     C
074100170626     C/EXEC SQL
074200170626     C+ declare C_R2 asensitive cursor for S_R2
074300170626     C/END-EXEC
074400170626     C
074500170626     C/EXEC SQL
074600170626     C+ open C_R2
074700170626     C/END-EXEC
074800170626     C
074900170626     C/EXEC SQL
075000170626     C+ Fetch C_R2 into :oD_DPT, :oD_SSORT, :oD_PTNC
075100170626     C/END-EXEC
075200170626     C*
075300170626     C                   if        sqlcod = *zeros
075400170626     C*
075500170626     C* Quindi aggancio i dati del DESTINATION DEPOT
075600170626     C
075700170627     C/EXEC SQL
075800170627     C+ SET :wBUFFER  = (SELECT
075900170627     C+               '1' CONCAT CDPISO2 CONCAT CDPDSTR CONCAT CDPIATA
076000170629     C+               CONCAT CDPGRPID
076100170627     C+               FROM DPCDP10F
076200170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
076300170627     C+               CDPDPTN = :oD_DPT
076400170627     C+               FETCH FIRST 1 ROW ONLY)
076500170626     C/END-EXEC
076600170626     C
076700170626     C*
076800170627     C                   if        %subst(wBUFFER:1:1) = *on
076900170628     C                   eval      DS_DCDP    = %subst(wBUFFER:2)
077000170627     C                   eval      oD_DPTISO2 = DS_DCDP.oD_DPTISO2
077100170627     C                   eval      oD_DSTR    = DS_DCDP.oD_DSTR
077200170627     C                   eval      oD_DPTIATA = DS_DCDP.oD_DPTIATA
077300170629     C                   eval      oD_GRPL    = DS_DCDP.oD_GRPL
077400170626     C                   else
077500170626     C                   eval      oD_DPTISO2 = oD_ISO2
077600170626     C                   endif
077700170626     C*
077800170626     C                   else
077900170628     C                   eval      wNotFoundCR2 = '1'
078000170626     C                   endif
078100170626     C
078200170626     C/EXEC SQL
078300170626     C+ close C_R2
078400170626     C/END-EXEC
078500170626     C
078600170626     C*
078700170628     C                   if        wNotFoundCR2 = *on
078800170626     C                   eval      OSID4ERR  = '1'
078900170626     C                   eval      OSID4ERRL = '96'
079000170626     C                   eval      OSID4ERRD = 'Errore in calcolo ' +
079100170626     C                             'instradamento R2 (DPCR210F)'
079200170626     C                   seton                                        70
079300170626     C                   endif
079400170626     C*
079500170626     C                   ENDSR
079600170626
079700170626
079800170626     C*------------------------------------------------------------------------*
079900170626     C* RTVCR3 - Esecuzione ricerca R3
080000170626     C*------------------------------------------------------------------------*
080100170626     C     RTVCR3        BEGSR
080200170626     C*
080300170626     C* Inizializzazioni
080400170626     C                   clear                   oD_OSORT
080500170626     C                   clear                   stringaSQL
080600170628     C                   clear                   wNotFoundCR3
080700170626     C*
080800170626     C* Compongo la stringa SQL
080900170626       stringaSQL =
081000170626       'SELECT CR3OSORT FROM DPCR310F ' +
081100170626       'WHERE CR3ATB=§ § and CR3VER='+%editc(sVERSION:'4')+' and ' +
081200170626       'CR3ISO2=§'+%trim(oD_ISO2)+'§ and ' +
081300170626       '(CR3DDPT=§ § or CR3DDPT=§'+oD_DPT+'§) and ' +
081400170626       '(CR3PTCB=§ § or ' +
081500170626       '§'+%trim(USID4CAD)+'§ between CR3PTCB and CR3PTCE) and ' +
081600170706       '(CR3NTWP=§ § or CR3NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
081700170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
081800170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
081900170626       ' (CR3ROUT=§ § and CR3ROUPB=§ §) or' +
082000170627       ' (CR3ROUT=§I§ and §'+oO_ICC+'§ between CR3ROUPB and CR3ROUPE) or'+
082100170627       ' (CR3ROUT=§C§ and CR3ROUPB=§'+oO_ISO2+'§) or ' +
082200170627       '(CR3ROUT=§D§ and §'+oO_DPT+'§ between CR3ROUPB and CR3ROUPE) or'+
082300170626       ' (CR3ROUT=§G§ and CR3ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
082400170627       ' (CR3ROUT=§B§ and CR3ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
082500170626       ') ' +
082600170626       'ORDER BY CR3PTY DESC ' +
082700170627       'FETCH FIRST 1 ROW ONLY ';
082800170627
082900170626     C*
083000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
083100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
083200170626     C
083300170626     C* Quindi eseguo stringa SQL così ottenuta
083400170626     C*
083500170626     C/EXEC SQL
083600170626     C+ prepare S_R3 from :stringaSQL
083700170626     C/END-EXEC
083800170626     C
083900170626     C/EXEC SQL
084000170626     C+ declare C_R3 asensitive cursor for S_R3
084100170626     C/END-EXEC
084200170626     C
084300170626     C/EXEC SQL
084400170626     C+ open C_R3
084500170626     C/END-EXEC
084600170626     C
084700170626     C/EXEC SQL
084800170626     C+ Fetch C_R3 into :oD_OSORT
084900170626     C/END-EXEC
085000170626     C*
085100170626     C                   if        sqlcod = *zeros
085200170626     C                   else
085300170628     C                   eval      wNotFoundCR3 = '1'
085400170626     C                   endif
085500170626     C
085600170626     C/EXEC SQL
085700170626     C+ close C_R3
085800170626     C/END-EXEC
085900170627     C
086000170626     C*
086100170626     C                   ENDSR
086200170627
086300170627
086400170627     C*------------------------------------------------------------------------*
086500170627     C* CHKALW - Verifica instradamenti consentiti ALLOW
086600170627     C*------------------------------------------------------------------------*
086700170627     C     CHKALW        BEGSR
086800170627     C*
086900170627     C* Inizializzazioni
087000170627     C                   clear                   oALLOWID
087100170627     C                   clear                   stringaSQL
087200170627     C                   clear                   wAllow
087300170627     C*
087400170627     C* Compongo la stringa SQL
087500170627       stringaSQL =
087600170627       'SELECT CWSALWID FROM DPCWS10F ' +
087700170627       'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
087800170629       '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
087900170724       // '(CWSRULFT=§N§ and CWSRULFB=§'+oO_GRPL+'§) or ' +     //
088000170629       '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
088100170629       '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
088200170629       '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
088300170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
088400170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and (' +
088500170629       %editc(ISID4SORC:'X')+' between CWSRULSB and CWSRULSE or ' +
088600170629       'CWSRULSB=0) and (' +
088700170629       '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
088800170629       '(CWSRULTT=§C§ and CWSRULTB=§'+oD_DPTISO2+'§) or ' +
088900170629       '(CWSRULTT=§N§ and CWSRULTB=§'+oD_NTWC+'§) or ' +
089000170629       '(CWSRULTT=§B§ and CWSRULTB=§'+%editc(oD_BUCN:'X')+oD_DPTISO2+'§) or ' +
089100170724       '(CWSRULTT=§G§ and CWSRULTB in (§'+%trim(oD_GRPL)+'§)) ) and (' +
089200170724       '(§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWSZONTB and CWSZONTE or ' +
089300170724       '(CWSZONTB=§ § and CWSZONTE=§ §))) ' +
089400170629
089500170627       'FETCH FIRST 1 ROW ONLY ';
089600170627
089700170627     C*
089800170627     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
089900170627     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
090000170627     C
090100170627     C* Quindi eseguo stringa SQL così ottenuta
090200170627     C*
090300170627     C/EXEC SQL
090400170627     C+ prepare S_WS from :stringaSQL
090500170627     C/END-EXEC
090600170627     C
090700170627     C/EXEC SQL
090800170627     C+ declare C_WS asensitive cursor for S_WS
090900170627     C/END-EXEC
091000170627     C
091100170627     C/EXEC SQL
091200170627     C+ open C_WS
091300170627     C/END-EXEC
091400170627     C
091500170627     C/EXEC SQL
091600170627     C+ Fetch C_WS into :oALLOWID
091700170627     C/END-EXEC
091800170627     C*
091900170627     C                   if        sqlcod = *zeros
092000170627     C                   eval      wAllow = '1'
092100170627     C                   endif
092200170627     C
092300170627     C/EXEC SQL
092400170627     C+ close C_WS
092500170627     C/END-EXEC
092600170627     C
092700170627     C                   if        wAllow = *on
092800170629     C*
092900170629     C* Se richiesto anche un ADDITIONAL SERVICE CODE => verifico se ALLOW
093000170629     C                   if        ISID4ASCO <> *blanks
093100170629     C                   exsr      CHKCWA
093200170629     C                   endif
093300170627     C                   else
093400170627     C                   eval      OSID4ERR  = '1'
093500170627     C                   eval      OSID4ERRL = '97'
093600170627     C                   eval      OSID4ERRD = 'Instradamento non ' +
093700170627     C                             'consentito. (DPCWS10F)'
093800170627     C                   seton                                        70
093900170627     C                   endif
094000170627     C*
094100170627     C                   ENDSR
094200170629
094300170629
094400170629     C*------------------------------------------------------------------------*
094500170629     C* CHKCWA - Verifica instradamenti consentiti ALLOW - ASCODE
094600170629     C*------------------------------------------------------------------------*
094700170629     C     CHKCWA        BEGSR
094800170629     C*
094900170629     C* Inizializzazioni
095000170629     C                   clear                   stringaSQL
095100170629     C                   clear                   wFound
095200170629     C*
095300170629     C* Compongo la stringa SQL
095400170629       stringaSQL =
095500170629       'SELECT §1§ FROM DPCWA10F ' +
095600170629       'WHERE CWAATB=§ § and CWAVER='+%editc(sVERSION:'4')+' and ' +
095700170629       'CWAALWID='+%editc(oALLOWID:'4')+' and (§' +
095800170629       ISID4ASCO+'§ between CWARULSB and CWARULSE or CWARULSB=§ §) and (' +
095900170629       '§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWAZONTB and CWAZONTE)) ' +
096000170629
096100170629       'FETCH FIRST 1 ROW ONLY ';
096200170629
096300170629     C*
096400170629     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
096500170629     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
096600170629     C
096700170629     C* Quindi eseguo stringa SQL così ottenuta
096800170629     C*
096900170629     C/EXEC SQL
097000170629     C+ prepare S_WA from :stringaSQL
097100170629     C/END-EXEC
097200170629     C
097300170629     C/EXEC SQL
097400170629     C+ declare C_WA asensitive cursor for S_WA
097500170629     C/END-EXEC
097600170629     C
097700170629     C/EXEC SQL
097800170629     C+ open C_WA
097900170629     C/END-EXEC
098000170629     C
098100170629     C/EXEC SQL
098200170629     C+ Fetch C_WS into :wFound
098300170629     C/END-EXEC
098400170629     C*
098500170629     C                   if        wFound = *on
098600170629     C                   else
098700170629     C                   eval      OSID4ERR  = '1'
098800170629     C                   eval      OSID4ERRL = '97'
098900170629     C                   eval      OSID4ERRD = 'Instradamento non ' +
099000170629     C                             'consentito. (DPCWA10F)'
099100170629     C                   seton                                        70
099200170629     C                   endif
099300170629     C
099400170629     C/EXEC SQL
099500170629     C+ close C_WS
099600170629     C/END-EXEC
099700170629     C
099800170629     C*
099900170629     C                   ENDSR
100000170626
100100170626
100200170626     C*------------------------------------------------------------------------*
100300170626     C* RTVCR4 - Esecuzione ricerca R4
100400170626     C*------------------------------------------------------------------------*
100500170626     C     RTVCR4        BEGSR
100600170626     C*
100700170626     C* Inizializzazioni
100800170626     C                   clear                   oD_CSORT
100900170626     C                   clear                   stringaSQL
101000170628     C                   clear                   wNotFoundCR4
101100170626     C*
101200170626     C* Compongo la stringa SQL
101300170626       stringaSQL =
101400170627       'SELECT CR4CSORT FROM DPCR410F ' +
101500170626       'WHERE CR4ATB=§ § and CR4VER='+%editc(sVERSION:'4')+' and ' +
101600170626       'CR4ISO2=§'+%trim(oD_ISO2)+'§ and ' +
101700170626       '(CR4DDPT=§ § or CR4DDPT=§'+oD_DPT+'§) and ' +
101800170706       '(CR4NTWP=§ § or CR4NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
101900170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
102000170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
102100170626       ' (CR4ROUT=§ § and CR4ROUPB=§ §) or' +
102200170627       ' (CR4ROUT=§I§ and §'+oO_ICC+'§ between CR4ROUPB and CR4ROUPE) or'+
102300170627       ' (CR4ROUT=§C§ and CR4ROUPB=§'+oO_ISO2+'§) or ' +
102400170627       '(CR4ROUT=§D§ and §'+oO_DPT+'§ between CR4ROUPB and CR4ROUPE) or'+
102500170626       ' (CR4ROUT=§G§ and CR4ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
102600170629       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
102700170627       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
102800170626       ') ' +
102900170626       'ORDER BY CR4PTY DESC ' +
103000170627       'FETCH FIRST 1 ROW ONLY ';
103100170627
103200170626     C*
103300170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
103400170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
103500170626     C
103600170626     C* Quindi eseguo stringa SQL così ottenuta
103700170626     C*
103800170626     C/EXEC SQL
103900170626     C+ prepare S_R4 from :stringaSQL
104000170626     C/END-EXEC
104100170626     C
104200170626     C/EXEC SQL
104300170626     C+ declare C_R4 asensitive cursor for S_R4
104400170626     C/END-EXEC
104500170626     C
104600170626     C/EXEC SQL
104700170626     C+ open C_R4
104800170626     C/END-EXEC
104900170626     C
105000170626     C/EXEC SQL
105100170626     C+ Fetch C_R4 into :oD_CSORT
105200170626     C/END-EXEC
105300170626     C*
105400170626     C                   if        sqlcod = *zeros
105500170626     C                   else
105600170628     C                   eval      wNotFoundCR4 = '1'
105700170626     C                   endif
105800170626     C
105900170626     C/EXEC SQL
106000170626     C+ close C_R4
106100170626     C/END-EXEC
106200170626     C
106300170626     C*
106400170626     C                   ENDSR
106500170626
106600170626
106700170626     C*------------------------------------------------------------------------*
106800170626     C* RTVCR5 - Esecuzione ricerca R5
106900170626     C*------------------------------------------------------------------------*
107000170626     C     RTVCR5        BEGSR
107100170626     C*
107200170626     C* Inizializzazioni
107300170626     C                   clear                   oD_DSORT
107400170626     C                   clear                   stringaSQL
107500170628     C                   clear                   wNotFoundCR5
107600170626     C*
107700170626     C* Compongo la stringa SQL
107800170626       stringaSQL =
107900170627       'SELECT CR5DSORT FROM DPCR510F ' +
108000170626       'WHERE CR5ATB=§ § and CR5VER='+%editc(sVERSION:'4')+' and ' +
108100170724       'CR5ISO2=§'+%trim(oD_ISO2)+'§ and ' +
108200170627       'CR5DDPT=§'+oD_DPT+'§ and ' +
108300170626       '(CR5PTCB=§ § or ' +
108400170626       '§'+%trim(USID4CAD)+'§ between CR5PTCB and CR5PTCE) and ' +
108500170706       '(CR5NTWP=§ § or CR5NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
108600170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
108700170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
108800170626       ' (CR5ROUT=§ § and CR5ROUPB=§ §) or' +
108900170627       ' (CR5ROUT=§I§ and §'+oO_ICC+'§ between CR5ROUPB and CR5ROUPE) or'+
109000170627       ' (CR5ROUT=§C§ and CR5ROUPB=§'+oO_ISO2+'§) or ' +
109100170627       '(CR5ROUT=§D§ and §'+oO_DPT+'§ between CR5ROUPB and CR5ROUPE) or'+
109200170626       ' (CR5ROUT=§G§ and CR5ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
109300170629       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
109400170627       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
109500170626       ') ' +
109600170626       'ORDER BY CR5PTY DESC ' +
109700170627       'FETCH FIRST 1 ROW ONLY ';
109800170627
109900170626     C*
110000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
110100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
110200170626     C
110300170626     C* Quindi eseguo stringa SQL così ottenuta
110400170626     C*
110500170626     C/EXEC SQL
110600170626     C+ prepare S_R5 from :stringaSQL
110700170626     C/END-EXEC
110800170626     C
110900170626     C/EXEC SQL
111000170626     C+ declare C_R5 asensitive cursor for S_R5
111100170626     C/END-EXEC
111200170626     C
111300170626     C/EXEC SQL
111400170626     C+ open C_R5
111500170626     C/END-EXEC
111600170626     C
111700170626     C/EXEC SQL
111800170626     C+ Fetch C_R5 into :oD_DSORT
111900170626     C/END-EXEC
112000170626     C*
112100170626     C                   if        sqlcod = *zeros
112200170626     C                   else
112300170628     C                   eval      wNotFoundCR5 = '1'
112400170626     C                   endif
112500170626     C
112600170626     C/EXEC SQL
112700170626     C+ close C_R5
112800170626     C/END-EXEC
112900170626     C
113000170626     C*
113100170626     C                   ENDSR
113200170627
113300170627
113400170627     C*------------------------------------------------------------------------*
113500170627     C* EXEOUT - Valorizzo paramteri di output
113600170627     C*------------------------------------------------------------------------*
113700170627     C     EXEOUT        BEGSR
113800170627     C*
113900170627     C                   eval      OSID4VER   = sVERSION
114000170627     C                   eval      OSID4VERD  = oVER_DPD
114100170627     C                   eval      OSID4VDDE  = CVEDDE
114200170627     C                   eval      OSID4DBUCN = oD_BUCN
114300170627     C                   eval      OSID4DBUCA = oD_BUCA
114400170627     C                   eval      OSID4DNTWC = oD_NTWC
114500170627     C                   eval      OSID4DDPTC = oD_DPTISO2
114600170627     C                   eval      OSID4DDPT  = oD_DPT
114700170627     C                   eval      OSID4DSTR  = oD_DSTR
114800170627     C                   eval      OSID4SSORT = oD_SSORT
114900170627     C                   eval      OSID4DPTNC = oD_PTNC
115000170627     C                   eval      OSID4OSORT = oD_OSORT
115100170627     C                   eval      OSID4CSORT = oD_CSORT
115200170627     C                   eval      OSID4DSORT = oD_DSORT
115300170627     C                   eval      OSID4BCID  = oVER_BCID
115400170627     C                   eval      OSID4SRVX  = oSRV_TXT
115500170627     C                   eval      OSID4SRVM  = oSRV_MRK
115600170627     C                   eval      OSID4ODPT  = oO_DPT
115700170627     C                   eval      OSID4ODPTC = oO_ISO2
115800170627     C                   eval      OSID4OBUCN = oO_BUCN
115900170627     C                   eval      OSID4OGRPL = oO_GRPL
116000170627     C                   eval      OSID4OICC  = oO_ICC
116100170627     C                   eval      OSID4ALWID = oALLOWID
116200170627     C                   eval      OSID4IATA  = oD_DPTIATA
116300170629     C                   evalr     OSID4CADP  = %trim(USID4CAD)
116400170629     C                   eval      OSID4CADP = %scanrpl(' ':'0':OSID4CADP)
116500170627     C*
116600170627     C* Sancisco esito a OK
116700170627     C                   movel     *blanks       OSID4ERR
116800170627     C*
116900170627     C                   ENDSR
117000170626
117100170626
117200170626     C*------------------------------------------------------------------------*
117300170626     C* CARTAB - Caricamemto tabelle si procedura
117400170626     C*------------------------------------------------------------------------*
117500170626     C     CARTAB        BEGSR
117600170626     C*
117700170626     C* Reperisco i valori tabellati di procedura
117800170626     C                   eval      tblKUT = 1
117900170626     C                   eval      tblCOD = '3I'
118000170626     C                   eval      tblKEY ='DPD'
118100170626     C     KEYtab00_C    chain     tabel00f
118200170626     C                   if        %found(tabel00f)
118300170626     C                   eval      DS3IDP = tblUNI
118400170627     C                   eval      gBU   = §3IBUCN
118500170627     C                   eval      gISO2 = §3IISO2
118600170626     C                   else
118700170626     C                   eval      OSID4ERR  = '1'
118800170627     C                   eval      OSID4ERRL = '80'
118900170627     C                   eval      OSID4ERRD = 'Tabella 3I valore §3IBUC non ' +
119000170626     C                             'trovato'
119100170626     C                   seton                                        70
119200170626     C                   endif
119300170626     C*
119400170626     C                   ENDSR
119500170620
119600170620
119700170620     C*--------------------------------------------------------------------------------------------*
119800060515     C* *inzsr - operazioni iniziali
119900060515     C*--------------------------------------------------------------------------------------------*
120000000000     C     *inzsr        BEGSR
120100060515     C*
120200060515     C* Ricevimento parametri
120300060515     C     *ENTRY        PLIST
120400170627     C                   PARM                    TISID4DS
120500060510     C*
120600060510     C* CALCOLA LA DATA CORRENTE
120700170620     C                   Z-ADD     *zeros        datcor            8 0
120800170620     C                   EVAL      datcor = %dec(%date() : *iso)
120900170626     C*
121000170626     C* CARICAMENTO TABELLE DI PROCEDURA
121100170626     C                   EXSR      CARTAB
121200170626     C*
121300170626     C* TABEL00F - Completa
121400170626     C     KEYtab00_C    KLIST
121500170626     C                   KFLD                    tblKUT
121600170626     C                   KFLD                    tblCOD
121700170626     C                   KFLD                    tblKEY
121800170622     C*
121900170622     C* DPCCC14I - Completa
122000170622     C     KEYccc14_C    KLIST
122100170622     C                   KFLD                    cccATB
122200170622     C                   KFLD                    cccVER
122300170622     C                   KFLD                    cccBRT
122400060515     C*
122500060515     C                   ENDSR
