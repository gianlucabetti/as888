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
001200180216     FDPCCC11I  IF   E           K DISK    rename(DPCCC100:DPCCC11)
001300180215      **  al posto degli SQL
001400180216     FDPCDP11i  IF   E           K DISK
001500180215     FDPCPC11i  IF   E           K DISK
001600180216     FDPCNT12i  IF   E           K DISK
001700180215     FDPCSO11i  IF   E           K DISK
001800180215     FDPCIC11i  IF   E           K DISK
001900180215     FDPCR113i  IF   E           K DISK
002000180215     FDPCR213i  IF   E           K DISK
002100180219     FDPCR313i  IF   E           K DISK
002200180219     FDPCR413i  IF   E           K DISK
002300180219     FDPCR513i  IF   E           K DISK
002400180219      *
002500180215     FDPCNP11i  IF   E           K DISK
002600180215     FDPCBU11i  IF   E           K DISK
002700180216     FDPCWS11i  IF   E           K DISK
002800180219     FDPCWA11i  IF   E           K DISK
002900180216      *
003000170620     D*------------------
003100170620     D* DS ESTERNE
003200170620     D*------------------
003300180215     D*** DPCSO10F      e ds
003400180215     D*** DPCDP10F      e ds
003500180215     D*** DPCIC10F      e ds
003600180215     D*** DPCBU10F      e ds
003700180216     D*** DPCNT10F      e ds
003800180215     D*** DPCR210F      e ds
003900180215     D*** DPCR310F      e ds
004000180216     D*** DPCWS10F      e ds
004100180219     D*** DPCR410F      e ds
004200180219     D*** DPCR510F      e ds
004300180216      *
004400180216     D DPCVE10F      e ds
004500170620     D TISID2DS      e ds
004600170622     D TISID4DS      e ds
004700170622     D OG143         e ds
004800170626     D DS3IDP        e ds
004900170620
005000170620     D*------------------
005100170620     D* VARIABILI D WRK
005200170620     D*------------------
005300170626     D stringaSQL      s           2048A   varying inz
005400170626     D api             s              1A   inz('''')
005500170626     D*
005600170626     D sDAT_RIF        s                   like(ISID4DRI)    inz
005700170626     D sVERSION        s                   like(OSID4VER)    inz
005800170626     D*
005900170627     D oVER_DPD        s                   like(CVEVERD)     inz
006000170627     D oVER_BCID       s                   like(CVEBCID)     inz
006100170627     D oSRV_TXT        s                   like(CSOSTXT)     inz
006200170627     D oSRV_MRK        s                   like(CSOSMRK)     inz
006300170627     D oALLOWID        s                   like(CWSALWID)    inz
006400170626     D*
006500170627     D oO_DPT          s                   like(CDPDPTN)     inz
006600170627     D oO_ISO2         s                   like(CDPISO2)     inz
006700170627     D oO_BUCN         s                   like(CDPBUCN)     inz
006800170627     D oO_GRPL         s                   like(CDPGRPID)    inz
006900170628     D oO_ICC          s                   like(CICICC)      inz
007000170626     D*
007100170627     D oD_ISO2         s                   like(CDPISO2)     inz
007200170627     D oD_BUCN         s                   like(CDPBUCN)     inz
007300170627     D oD_BUCA         s                   like(CBUBUCA)     inz
007400170627     D oD_NTWC         s                   like(CNTNTWC)     inz
007500170628     D oD_ISON         s                   like(CCCISON)     inz
007600170629     D oD_GRPL         s                   like(CDPGRPID)    inz
007700170626     D*
007800170627     D oD_DPT          s                   like(CR2DDPT)     inz
007900170627     D oD_DPTISO2      s                   like(CDPISO2)     inz
008000170627     D oD_DSTR         s                   like(CDPDSTR)     inz
008100170627     D oD_SSORT        s                   like(CR2SSORT)    inz
008200170627     D oD_PTNC         s                   like(CR2PTNC)     inz
008300170627     D oD_DPTIATA      s                   like(CDPIATA)     inz
008400170626     D*
008500170627     D oD_OSORT        s                   like(CR3OSORT)    inz
008600170627     D oD_CSORT        s                   like(CR4CSORT)    inz
008700170627     D oD_DSORT        s                   like(CR5DSORT)    inz
008800170626     D*
008900170626     D gBU             s                   like(§3IBUCN) inz
009000170627     D gISO2           s                   like(§3IISO2) inz
009100180110     D SkLNP           s              3S 0 dim(500)inz
009200180110     D SkDPT           s              7A   dim(500)inz
009300170622     D jLNP            s              3S 0 inz
009400170622     D sjLNP           s                   like(jLNP) inz
009500180110     D SkNZD           s              3A   dim(500)inz
009600180110     D SkISO2          s              2A   dim(500)inz
009700170622     D jNZD            s              3S 0 inz
009800170622     D sjNZD           s                   like(jNZD) inz
009900170622     D wFound          s              1A   inz
010000170628     D wNotFoundCR1    s              1A   inz
010100170628     D wNotFoundCR2    s              1A   inz
010200170628     D wNotFoundCR3    s              1A   inz
010300170628     D wNotFoundCR4    s              1A   inz
010400170628     D wNotFoundCR5    s              1A   inz
010500170627     D wAllow          s              1A   inz
010600170626     D*
010700180110     D primo           s              2A   inz
010800180110     D*
010900170627     D wBUFFER         s            256A   inz
011000170629     D DS_CSO          ds                  qualified inz
011100170627     D   oSRV_TXT                          like(oSRV_TXT)
011200170627     D   oSRV_MRK                          like(oSRV_MRK)
011300170626     D*
011400170627     D DS_OCDP         ds                  qualified inz
011500170627     D   oO_ISO2                           like(oO_ISO2)
011600170627     D   oO_BUCN                           like(oO_BUCN)
011700170627     D   oO_GRPL                           like(oO_GRPL)
011800170626     D*
011900170627     D DS_DCDP         ds                  qualified inz
012000170627     D   oD_DPTISO2                        like(oD_DPTISO2)
012100170627     D   oD_DSTR                           like(oD_DSTR)
012200170627     D   oD_DPTIATA                        like(oD_DPTIATA)
012300170629     D   oD_GRPL                           like(oD_GRPL)
012400170620
012500170620     C*--------------------------------------------------------------------------------------------*
012600170620     C* Main lines
012700170620     C*--------------------------------------------------------------------------------------------*
012800150518     C*
012900150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013000150518     C
013100180221
013200180216     c                   goto      no_sql
013300150518     C/EXEC SQL
013400150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013500150518     C/END-EXEC
013600180216     c     no_sql        tag
013700170620     C*
013800170620     C* Avvio il monitoring del intero flusso
013900180223     C                   monitor
014000170622     C*
014100170622     C* Resetto indicatore d errore
014200170622     C                   setoff                                       70
014300170620     C*
014400170620     C* Inizializzazioni
014500170622     C                   exsr      exeINZ
014600170622     C*
014700170622     C* Step 01 - Verifica parametri di input
014800170626     C  N70              exsr      chkPAR
014900170622     C*
015000170622     C* Step 02 - Reperimento dati Versione
015100170622     C  N70              exsr      rtvVER
015200170622     C*
015300170626     C* Step 03 - Reperimento Depot DPD (partenza)
015400170626     C  N70              exsr      rtvDPT
015500170622     C*
015600170626     C* Step 04 - Reperimento DESTINATION Nazione ISO 2
015700170626     C  N70              exsr      rtvDISO2
015800170622     C*
015900170622     C* Step 05 - Verifica del Pattern CAP
016000170622     C  N70              exsr      chkPATT
016100170622     C*
016200170622     C* Step 06 - Verifica esistenza CAP in DPD
016300170622     C  N70              exsr      chkPTC
016400170622     C*
016500170622     C* Step 07 - Reperimento dati SOCODE
016600170622     C  N70              exsr      rtvSORC
016700170622     C*
016800170622     C* Step 08 - Reperimento dati ORIGIN DEPOT
016900170626     C  N70              exsr      rtvODPT
017000170626     C*
017100170626     C* Step 09 - Reperimento dati INJECTION CODE
017200170626     C  N70              exsr      rtvCIC
017300170626     C*
017400170626     C* Step 10 - Esecuzione ricerca R1
017500170626     C  N70              exsr      rtvCR1
017600170626     C*
017700170626     C* Step 11 - Esecuzione ricerca R2
017800170626     C  N70              exsr      rtvCR2
017900170626     C*
018000170627     C* Step 12 - Verifica instradamenti consentiti ALLOW
018100170627     C  N70              exsr      chkALW
018200170626     C*
018300170626     C* Step 13 - Esecuzione ricerca R3
018400170626     C  N70              exsr      rtvCR3
018500170626     C*
018600170626     C* Step 14 - Esecuzione ricerca R4
018700170626     C  N70              exsr      rtvCR4
018800170626     C*
018900170626     C* Step 15 - Esecuzione ricerca R5
019000170626     C  N70              exsr      rtvCR5
019100170620     C*
019200170627     C* Step 16 - Valorizzo paramteri di output
019300170627     C  N70              exsr      exeOUT
019400170620     C*
019500170620     C* Gestisco eventuale errore
019600180223     C                   on-error
019700180223     C                   dump(A)
019800180223     C                   eval      OSID4ERR = 'E'
019900170620     C*
020000170620     C* Arresto il monitoring
020100180223     C                   endmon
020200170620     C*
020300180131     C*********          seton                                        rt
020400060515     C*
020500180131     C                   seton                                        LR
020600170622     C*------------------------------------------------------------------------*
020700170622     C* EXEINZ - Inizializzazioni
020800170622     C*------------------------------------------------------------------------*
020900170627     C     EXEINZ        BEGSR
021000170622     C*
021100170627     C* Variabili di work
021200170627     C                   clear                   oD_BUCN
021300170627     C                   clear                   oD_BUCA
021400170627     C                   clear                   oD_NTWC
021500170628     C                   clear                   oD_ISON
021600170627     C                   clear                   oD_DPTISO2
021700170627     C                   clear                   oD_DPT
021800170627     C                   clear                   oD_DSTR
021900170627     C                   clear                   oD_SSORT
022000170627     C                   clear                   oD_PTNC
022100170627     C                   clear                   oD_OSORT
022200170627     C                   clear                   oD_CSORT
022300170627     C                   clear                   oD_DSORT
022400170627     C                   clear                   oSRV_TXT
022500170627     C                   clear                   oSRV_MRK
022600170627     C                   clear                   oO_DPT
022700170627     C                   clear                   oO_ISO2
022800170627     C                   clear                   oO_BUCN
022900170627     C                   clear                   oO_GRPL
023000170627     C                   clear                   oO_ICC
023100170627     C                   clear                   oALLOWID
023200170627     C                   clear                   oD_DPTIATA
023300170627     C*
023400170627     C* Paramteri di output
023500170629     C                   clear                   USID4CAD
023600170627     C                   clear                   OSID4VER
023700170627     C                   clear                   OSID4VERD
023800170627     C                   clear                   OSID4VDDE
023900170627     C                   clear                   OSID4DBUCN
024000170627     C                   clear                   OSID4DBUCA
024100170627     C                   clear                   OSID4DNTWC
024200170627     C                   clear                   OSID4DDPTC
024300170627     C                   clear                   OSID4DDPT
024400170627     C                   clear                   OSID4DSTR
024500170627     C                   clear                   OSID4SSORT
024600170627     C                   clear                   OSID4DPTNC
024700170627     C                   clear                   OSID4OSORT
024800170627     C                   clear                   OSID4CSORT
024900170627     C                   clear                   OSID4DSORT
025000170627     C                   clear                   OSID4BCID
025100170627     C                   clear                   OSID4SRVX
025200170627     C                   clear                   OSID4SRVM
025300170627     C                   clear                   OSID4ODPT
025400170627     C                   clear                   OSID4ODPTC
025500170627     C                   clear                   OSID4OBUCN
025600170627     C                   clear                   OSID4OGRPL
025700170627     C                   clear                   OSID4OICC
025800170627     C                   clear                   OSID4ALWID
025900170629     C                   clear                   OSID4IATA
026000170629     C                   clear                   OSID4CADP
026100170627     C                   clear                   OSID4ERRL
026200170627     C                   clear                   OSID4ERRD
026300170622     C*
026400170627     C* Inizializzo l'esito
026500170627     C                   movel     '0'           OSID4ERR
026600170627     C*
026700170622     C                   ENDSR
026800170622     C*------------------------------------------------------------------------*
026900170622     C* CHKPAR - Verifica parametri di input
027000170622     C*------------------------------------------------------------------------*
027100170622     C     CHKPAR        BEGSR
027200170622     C*
027300170622     C                   setoff                                       40
027400170622     C*
027500170622     C* Data di riferimento
027600170622     C                   if        ISID4DRI = *zeros
027700170626     C                   eval      ISID4DRI = datcor
027800170622     C                   endif
027900170622     C*
028000170622     C* Linea di Partenza
028100170622     C                   if        ISID4LNP = *zeros
028200170622     C                   seton                                        40
028300170622     C                   endif
028400170622     C*
028500170622     C* CAP destinatario
028600170622     C                   if        ISID4CAD = *blanks
028700170622     C                   seton                                        40
028800170629     C                   else
028900170629     C                   eval      USID4CAD = ISID4CAD
029000170622     C                   endif
029100170629     C*
029200170629     C* Forzatura su CAP destinatario per NAZIONE IRLANDA
029300170629     C                   if        ISID4NZD = 'IRL'
029400180108     C                             and ISID4CAD <>'2'
029500170629     C                   eval      USID4CAD = '1'
029600170629     C                   endif
029700170622     C*
029800170622     C* Servizio DPD
029900170622     C                   if        ISID4SORC = *zeros
030000170622     C                   seton                                        40
030100170622     C                   endif
030200170622     C*
030300170622     C* Se nn ok => imposto subito l'errore generale d procedura
030400170622     C                   if        *in40
030500170622     C                   eval      OSID4ERR  = '1'
030600170622     C                   eval      OSID4ERRL = '92'
030700170622     C                   eval      OSID4ERRD = 'Errore: parametri obbligatori '+
030800170622     C                             'non valorizzati'
030900170622     C                   seton                                        70
031000170622     C                   endif
031100170622     C*
031200170622     C                   ENDSR
031300170622     C*------------------------------------------------------------------------*
031400170622     C* RTVVER - Reperimento VERSIONE richiesta
031500170622     C*------------------------------------------------------------------------*
031600170622     C     RTVVER        BEGSR
031700170622     C*
031800170622     C* Reperisco VERSIONE solo se non ancora reperita o data riferimento cambiata
031900170626     C                   if        sVERSION  = *zeros    OR
032000170626     C                             ISID4DRI <> sDAT_RIF
032100170626     C*
032200170626     C                   eval      sDAT_RIF = ISID4DRI
032300170622     C*
032400170622     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
032500170627     C                   call      'TISID1R'
032600170626     C                   parm                    sDAT_RIF
032700170626     C                   parm                    sVERSION
032800170627     C                   parm                    oVER_DPD
032900170627     C                   parm                    CVEDDE
033000170627     C                   parm                    CVEDSC
033100170626     C                   parm                    oVER_BCID
033200170622     C*
033300170622     C* Se reperita versione valida
033400170626     C                   if        sVERSION  > *zeros
033500170622     C                   else
033600170622     C*
033700170622     C* Se non ok => imposto subito l'errore generale d procedura
033800170622     C                   eval      OSID4ERR  = '1'
033900170622     C                   eval      OSID4ERRL = '90'
034000170622     C                   eval      OSID4ERRD = 'Errore in reperimento versione'+
034100170622     C                             ' (DPCVE10F)'
034200170622     C                   seton                                        70
034300170622     C                   endif
034400170622     C                   endif
034500170622     C*
034600170622     C                   ENDSR
034700170622     C*------------------------------------------------------------------------*
034800170626     C* RTVDPT - Reperimento DEPOT DPD (partenza)
034900170622     C*------------------------------------------------------------------------*
035000170626     C     RTVDPT        BEGSR
035100170622     C*
035200170626     C                   clear                   oO_DPT
035300170622     C*
035400170622     C* Cerco abbinamento Filiale BRT <=> Depot DPD
035500170627     C                   z-add     1             jLNP
035600170622     C     ISID4LNP      lookup    SkLNP(jLNP)                            55
035700170622     C                   if        %found
035800170626     C                   eval      oO_DPT = SkDPT(jLNP)
035900170622     C                   else
036000170622     C*
036100170622     C     ISID4LNP      chain     azorg01l
036200170622     C                   if        %found(azorg01l)
036300170622     C                   eval      OG143 = ORGDE3
036400170622     C                   add       1             sjLNP
036500170626     C                   eval      SkLNP(sjLNP) = ISID4LNP
036600170627     C                   eval      SkDPT(sjLNP) = %editc(gBU:'X')+§OGDP1
036700170626     C                   eval      oO_DPT = SkDPT(sjLNP)
036800170622     C                   endif
036900170622     C*
037000170626     C                   if        oO_DPT = *blanks
037100170622     C                   eval      OSID4ERR  = '1'
037200170622     C                   eval      OSID4ERRL = '91'
037300170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
037400170622     C                             'partenza DPD (AZORG01L)'
037500170622     C                   seton                                        70
037600170622     C                   endif
037700170622     C                   endif
037800170622     C*
037900170622     C                   ENDSR
038000170622     C*------------------------------------------------------------------------*
038100170626     C* RTVDISO2 - Reperimento DESTINATION CODICE NAZIONE ISO 2
038200170622     C*------------------------------------------------------------------------*
038300170626     C     RTVDISO2      BEGSR
038400170622     C*
038500170622     C                   clear                   oD_ISO2
038600170622     C*
038700170622     C* Cerco abbinamento Cod Nazione BRT <=> Codice Nazione ISO2
038800170627     C                   z-add     1             jNZD
038900170622     C     ISID4NZD      lookup    SkNZD(jNZD)                            55
039000180110     C                   if        %found AND
039100180110     C                             (ISID4NZD <> *blanks or
039200180110     C                              ISID4NZD  = *blanks and primo ='NO')
039300170630     C                   eval      oD_ISO2 = SkISO2(jNZD)
039400170622     C                   else
039500170622     C*
039600170622     C                   eval      cccATB = *blanks
039700170626     C                   eval      cccVER = sVERSION
039800170622     C                   eval      cccBRT = ISID4NZD
039900170622     C     KEYccc14_C    chain     dpccc14i
040000170622     C                   if        %found(dpccc14i)
040100180110      *
040200180110      *   se ha caricato la prima volta il blank come 'IT'
040300180110     c                   if        primo = *blank and ISID4NZD = *blanks
040400180110     c                   eval      primo = 'NO'
040500180110     C                   endif
040600180110      *
040700170622     C                   add       1             sjNZD
040800170622     C                   eval      SkNZD(sjNZD)  = ISID4NZD
040900170622     C                   eval      SkISO2(sjNZD) = cccISO2
041000170628     C                   eval      oD_ISO2 = SkISO2(sjNZD)
041100170622     C                   endif
041200170622     C*
041300170622     C                   if        oD_ISO2 = *blanks
041400170622     C                   eval      OSID4ERR  = '1'
041500170622     C                   eval      OSID4ERRL = '93'
041600170622     C                   eval      OSID4ERRD = 'Errore in reperimento nazione '+
041700170622     C                             'destino (DPCCC10F)'
041800170622     C                   seton                                        70
041900170622     C                   endif
042000170622     C                   endif
042100170622     C*
042200170622     C                   ENDSR
042300170620     C*------------------------------------------------------------------------*
042400170620     C* CHKPATT - Verifica del Pattern CAP
042500170620     C*------------------------------------------------------------------------*
042600170620     C     CHKPATT       BEGSR
042700170622     C*
042800170622     C* Inizializzazioni
042900170622     C                   clear                   TISID2DS
043000170622     C*
043100170622     C* Valorizzazioni
043200170627     C                   eval      ISID2VER   = sVERSION
043300170627     C                   eval      ISID2NISO2 = oD_ISO2
043400170629     C                   eval      ISID2PTC   = USID4CAD
043500170622     C                   call      'TISID2R'
043600170622     C                   parm                    TISID2DS
043700170622     C*
043800170622     C                   if        OSID2ESI  = 'V'
043900170622     C                   eval      USID4CAD = OSID2PTC
044000170622     C                   else
044100170622     C                   eval      OSID4ERR  = '1'
044200170627     C                   eval      OSID4ERRL = '99'
044300170627     C                   eval      OSID4ERRD = 'Errore - CAP errato ' +
044400170622     C                             '(Pattern DPCCC10F)'
044500170622     C                   seton                                        70
044600170622     C                   endif
044700170620     C*
044800170620     C                   ENDSR
044900170622     C*------------------------------------------------------------------------*
045000170622     C* CHKPTC - Verifica esistenza CAP in DPD
045100170622     C*------------------------------------------------------------------------*
045200170622     C     CHKPTC        BEGSR
045300170622     C*
045400170622     C* Inizializzazioni
045500170622     C                   movel     *blanks       wFound
045600170629     C*
045700170629     C* Se necessaria validazione del CAP rispetto alla NAZIONE
045800170629     C                   if        OSID2PVAL = 'VB'                             * blocked on failure
045900170629     C
046000180215EL    *---------  da eliminare
046100180215     c                   goto      dopo_sql1
046200170622     C/EXEC SQL
046300170622     C+ SET :wFound = (SELECT '1' FROM DPCPC10F WHERE CPCATB = ' ' AND
046400180216     C+                CPCVER = :sVERSION AND CPCISO2 = :oD_ISO2
046500170626     C+                AND :USID4CAD BETWEEN CPCPTCB AND CPCPTCE
046600170626     C+                FETCH FIRST 1 ROW ONLY)
046700170622     C/END-EXEC
046800180215     c     dopo_sql1     tag
046900180215EL    *---------  da eliminare
047000180215     c                   clear                   cpcATB
047100180215     C                   eval      cpcver = sVERSION
047200180215     C                   eval      cpciso2 = oD_ISO2
047300180215     c                   eval      cpcPTCB = USID4CAD
047400180215     c     kCPCset       setll     DPCPC11i
047500180215     c     kCPC          reade     DPCPC11i
047600180216     c                   dow       not %EoF(DPCPC11i)
047700180215      *
047800180215      * controlla se sta nel range
047900180215     c                   if        CPCPTCB <= USID4CAD and
048000180215     c                             CPCPTCE >= USID4CAD
048100180215     c                   eval      wFOUND = '1'
048200180215     c                   leave
048300180215     c                   endif
048400180215      * inoltre però,
048500180215      * se superato l'estremo, deve comunque uscire
048600180215     c                   if        CPCPTCE >= USID4CAD
048700180215     c                   leave
048800180215     c                   endif
048900180215     c     kCPC          reade     DPCPC11i
049000180215     c                   enddo
049100170622     C*
049200170622     C                   if        wFound = *on
049300170622     C                   else
049400170622     C                   eval      OSID4ERR  = '1'
049500170630     C                   eval      OSID4ERRL = '96'
049600170627     C                   eval      OSID4ERRD = 'Errore - CAP non trovato ' +
049700170622     C                             '(DPCPC10F)'
049800170622     C                   seton                                        70
049900170622     C                   endif
050000170629     C*
050100170629     C                   endif
050200170622     C*
050300170622     C                   ENDSR
050400170622     C*------------------------------------------------------------------------*
050500170622     C* RTVSORC - Reperimento dati SOCODE
050600170622     C*------------------------------------------------------------------------*
050700170622     C     RTVSORC       BEGSR
050800170622     C*
050900170622     C* Inizializzazioni
051000170627     C                   clear                   DS_CSO
051100170622     C
051200180215EL    *---------  da eliminare
051300180215     c                   goto      dopo_sql2
051400180220     C                   clear                   wBUFFER
051500170622     C/EXEC SQL
051600170629     C+ SET :wBUFFER = (SELECT '1' CONCAT CSOSTXT CONCAT CSOSMRK FROM DPCSO10F
051700170627     C+              WHERE CSOATB = ' ' AND CSOVER = :sVERSION AND
051800170627     C+              CSOSORC = :ISID4SORC
051900170627     C+              FETCH FIRST 1 ROW ONLY)
052000170622     C/END-EXEC
052100180215     c     dopo_sql2     tag
052200180215EL    *---------  da eliminare
052300180215     c                   clear                   csoATB
052400180215     c                   eval      csoVER = sVERSION
052500180215     c                   eval      csoSORC = ISID4SORC
052600180215     c     kCSO          chain     DPCSO11i
052700180215     c                   if        %Found(DPCSO11i)
052800180220     C                   eval      oSRV_TXT = csoSTXT
052900180220     C                   eval      oSRV_MRK = csoSMRK
053000170622     C                   else
053100170622     C                   eval      OSID4ERR  = '1'
053200170627     C                   eval      OSID4ERRL = '95'
053300170627     C                   eval      OSID4ERRD = 'Errore in reperimento servizio'+
053400170622     C                             ' (DPCSC10F)'
053500170622     C                   seton                                        70
053600170622     C                   endif
053700170622     C*
053800170622     C                   ENDSR
053900170622     C*------------------------------------------------------------------------*
054000170626     C* RTVODPT - Reperimento dati ORIGIN DEPOT
054100170622     C*------------------------------------------------------------------------*
054200170626     C     RTVODPT       BEGSR
054300170622     C*
054400170622     C* Inizializzazioni
054500170627     C                   clear                   DS_OCDP
054600170622     C
054700180215EL    *---------  da eliminare
054800180215     c                   goto      dopo_sql3
054900180220     C                   clear                   wBUFFER
055000170622     C/EXEC SQL
055100170627     C+ SET :wBUFFER  = (SELECT
055200170627     C+               '1' CONCAT CDPISO2 CONCAT digits(CDPBUCN) CONCAT CDPGRPID
055300170627     C+               FROM DPCDP10F
055400170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
055500170627     C+               CDPDPTN = :oO_DPT
055600170627     C+               FETCH FIRST 1 ROW ONLY)
055700170622     C/END-EXEC
055800180215     c     dopo_sql3     tag
055900180215EL    *---------  da eliminare
056000180215     c                   clear                   cdpATB
056100180215     c                   eval      cdpVER = sVERSION
056200180216     c                   eval      cdpDPTN = oO_DPT
056300180215     c     kCDP          chain     DPCDP11i
056400180215     c                   if        %Found(DPCDP11i)
056500180220     C                   eval      oO_ISO2 = cdpISO2
056600180220     C                   eval      oO_BUCN = cdpBUCN
056700180220     C                   eval      oO_GRPL = cdpGRPid
056800170622     C                   else
056900170622     C                   eval      OSID4ERR  = '1'
057000170626     C                   eval      OSID4ERRL = '91'
057100170622     C                   eval      OSID4ERRD = 'Errore in reperimento depot ' +
057200170622     C                             'partenza DPD (DPCDP10F)'
057300170622     C                   seton                                        70
057400170622     C                   endif
057500170622     C*
057600170622     C                   ENDSR
057700170626     C*------------------------------------------------------------------------*
057800170626     C* RTVCIC - Reperimento dati INJECTION CODE
057900170626     C*------------------------------------------------------------------------*
058000170626     C     RTVCIC        BEGSR
058100170626     C*
058200170626     C* Inizializzazioni
058300170626     C*
058400170626     C* Se indicato uns specifico SUBCODE in input
058500170626     C                   if        ISID4SUBC <> *blanks
058600170627     C                   eval      oO_ICC = oO_DPT + ISID4SUBC
058700180215     C
058800180215EL    *---------  da eliminare
058900180215     c                   goto      dopo_sql4
059000180220     C                   clear                   wBUFFER
059100170626     C/EXEC SQL
059200170627     C+ SET :wBUFFER  = (SELECT '1' CONCAT CICICC
059300170627     C+               FROM DPCIC10F
059400170627     C+               WHERE CICATB = ' ' AND CICVER = :sVERSION AND
059500170627     C+               CICICC = :oO_ICC
059600170627     C+               FETCH FIRST 1 ROW ONLY)
059700170626     C/END-EXEC
059800180215     c     dopo_sql4     tag
059900180215EL    *---------  da eliminare
060000180215     c                   clear                   cicATB
060100180215     c                   eval      cicVER = sVERSION
060200180215     c                   eval      cicICC = oO_ICC
060300180215     c     kCIC          chain     DPCIC11i
060400180220     c                   if        not %Found(DPCIC11i)
060500170626     C                   eval      OSID4ERR  = '1'
060600170626     C                   eval      OSID4ERRL = '94'
060700170626     C                   eval      OSID4ERRD = 'Errore in reperimento ' +
060800170626     C                             'INJECTION CODE (DPCIC10F)'
060900170626     C                   seton                                        70
061000170626     C                   endif
061100170626     C*
061200170626     C                   endif
061300170626     C*
061400170626     C                   ENDSR
061500170626     C*------------------------------------------------------------------------*
061600170626     C* RTVCR1 - Esecuzione ricerca R1
061700170626     C*------------------------------------------------------------------------*
061800170626     C     RTVCR1        BEGSR
061900170626     C*
062000170626     C* Inizializzazioni
062100170626     C                   clear                   oD_BUCN
062200170626     C                   clear                   oD_BUCA
062300170628     C                   clear                   oD_NTWC
062400170628     C                   clear                   oD_ISON
062500170626     C                   clear                   stringaSQL
062600170628     C                   clear                   wNotFoundCR1
062700170626     C*
062800180215EL    *---------  da eliminare
062900180215     c                   goto      dopo_sql5
063000170626     C* Compongo la stringa SQL
063100170626       stringaSQL =
063200170626       'SELECT CR1BUCN FROM DPCR110F ' +
063300170626       'WHERE CR1ATB=§ § and CR1VER='+%editc(sVERSION:'4')+' and ' +
063400170627       'CR1TRADS=§'+ISID4TRADS+'§ and ' +
063500170626       'CR1ISO2=§'+%trim(oD_ISO2)+'§ and (CR1PTCB = § § or ' +
063600170626       '§'+%trim(USID4CAD)+'§ between CR1PTCB and CR1PTCE) and ' +
063700170706       '(CR1NTWP=§ § or CR1NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
063800170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
063900170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
064000170626       ' (CR1ROUT=§ § and CR1ROUPB=§ §) or' +
064100170627       ' (CR1ROUT=§I§ and §'+oO_ICC+'§ between CR1ROUPB and CR1ROUPE) or'+
064200170627       ' (CR1ROUT=§C§ and CR1ROUPB=§'+oO_ISO2+'§) or ' +
064300170627       '(CR1ROUT=§D§ and §'+oO_DPT+'§ between CR1ROUPB and CR1ROUPE) or'+
064400170626       ' (CR1ROUT=§G§ and CR1ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
064500170627       ' (CR1ROUT=§B§ and CR1ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
064600170626       ') ' +
064700170626       'ORDER BY CR1PTY DESC ' +
064800170627       'FETCH FIRST 1 ROW ONLY ';
064900170626     C*
065000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
065100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
065200170626     C
065300170626     C* Quindi eseguo stringa SQL così ottenuta
065400170626     C*
065500170626     C/EXEC SQL
065600170626     C+ prepare S_R1 from :stringaSQL
065700170626     C/END-EXEC
065800170626     C
065900170626     C/EXEC SQL
066000170626     C+ declare C_R1 asensitive cursor for S_R1
066100170626     C/END-EXEC
066200170626     C
066300170626     C/EXEC SQL
066400170626     C+ open C_R1
066500170626     C/END-EXEC
066600170626     C
066700170626     C/EXEC SQL
066800170626     C+ Fetch C_R1 into :oD_BUCN
066900170626     C/END-EXEC
067000180215
067100180215     c     dopo_sql5     tag
067200180215EL    *---------  da eliminare
067300180215     C*
067400180215     C                   clear                   cr1_found         1
067500180215     C                   clear                   cr1ATB
067600180215     C                   eval       cr1VER   = sVERSION
067700180215     C                   eval       cr1TRADS = ISID4TRADS
067800180215     C                   eval       cr1ISO2  = oD_ISO2
067900180215      *
068000180215     C*   legge al contrario per la Priorità  DESCEND
068100180215     c     kCR1          setgt     DPCR113i
068200180215     c     kCR1          readpe    DPCR113i
068300180215     c                   dow        not %EoF(DPCR113i)
068400180215     C                   if        (CR1PTCB = ' ' or
068500180215     c                              CR1PTCB<= USID4CAD and CR1PTCE>= USID4CAD)
068600180216     c                             and (
068700180216     c                              (CR1ROUT=' ' and CR1ROUPB=' ') or
068800180216     c                              (CR1ROUT='C' and CR1ROUPB=oO_ISO2) or
068900180216     c                               (CR1ROUT='I' and
069000180216     c                              CR1ROUPB<= oO_ICC and CR1ROUPE>= oO_ICC) or
069100180216     c                              (CR1ROUT='D' and
069200180216     c                              CR1ROUPB<= oO_DPT and CR1ROUPE>= oO_DPT) or
069300180216     c                              (CR1ROUT='B' and
069400180216     c                              CR1ROUPB = %editc(oO_BUCN:'X')) or
069500180216     c                              (CR1ROUT='G' and CR1ROUPB = oO_GRPL)
069600180215     c                              )
069700180215     C*  o blank
069800180215     c                   if        CR1NTWP = ' '
069900180215     C                   eval       oD_BUCN   = CR1BUCN
070000180215     C                   eval       cr1_found ='1'
070100180215     c                   leave
070200180215     c                   else
070300180215      *- o verificato
070400180215     c                   eval        CNPATB  = ' '
070500180215     c                   eval        CNPVER  = sVERSION
070600180215     c                   eval        CNPNTWP = CR1NTWP
070700180221     c                   eval        CNPSORCB = ISID4SORC
070800180221     c     kCNP1         setll     dpcnp11i
070900180215     c     kCNP          reade     dpcnp11i
071000180215     c                   dow       not %EoF(dpcnp11i)
071100180215      *-
071200180221     c                   if         CNPSORCE >= ISID4SORC
071300180215     C                   eval       oD_BUCN   = CR1BUCN
071400180215     C                   eval       cr1_found ='1'
071500180215     c                   leave
071600180215     c                   endif
071700180215      *-
071800180215     c     kCNP          reade     dpcnp11i
071900180215     c                   enddo
072000180215      *-
072100180215      *- se trovato deve uscire dal ciclo principale
072200180215     C                   if         cr1_found ='1'
072300180215     c                   leave
072400180215     c                   endif
072500180215      *-
072600180215     c                   endif
072700180216     c                   endif
072800180215     c     kCR1          readpe    DPCR113i
072900180215     c                   enddo
073000170626     C*
073100180215     C*****                if        sqlcod = *zeros
073200180215     C                   if        cr1_found ='1'
073300170626     C*
073400170626     C* Quindi aggancio i dati della DESTINATION BUSINESS UNIT
073500180215EL    *---------  da eliminare
073600180215     C                   goto      dopo_sql6
073700170626     C/EXEC SQL
073800170626     C+ SET :oD_BUCA = (SELECT CBUBUCA FROM DPCBU10F
073900170626     C+                 WHERE CBUATB = ' ' AND CBUVER = :sVERSION AND
074000170627     C+                 CBUBUCN = :oD_BUCN
074100170627     C+                 FETCH FIRST 1 ROW ONLY)
074200170626     C/END-EXEC
074300170626     C                   if        sqlcod <> *zeros
074400170628     C                   eval      wNotFoundCR1 = '1'
074500170626     C                   endif
074600180215     C*
074700180215     c     dopo_sql6     tag
074800180215EL    *---------  da eliminare
074900180215     C*
075000180215     c                   clear                   cbuATB
075100180215     c                   eval      cbuVER  = sVERSION
075200180215     c                   eval      cbuBUCN = oD_BUCN
075300180215     c     kCBU          chain     DPCBU11i
075400180215     c                   if        not %Found(DPCBU11i)
075500180215     C                   eval      wNotFoundCR1 = '1'
075600180215     C                   else
075700180215     C                   eval      oD_BUCA = CBUBUCA
075800180215     C                   end
075900170626     C*
076000170626     C* Quindi aggancio i dati del DESTINATION NETWORK CODE
076100170626     C
076200180215EL    *---------  da eliminare
076300180215     C                   goto      dopo_sql7
076400170626     C/EXEC SQL
076500170626     C+ SET :oD_NTWC = (SELECT CNTNTWC FROM DPCNT10F
076600170626     C+                 WHERE CNTATB = ' ' AND CNTVER = :sVERSION AND
076700170626     C+                 CNTISO2 = :oD_ISO2 AND CNTBUCN = :oD_BUCN
076800170626     C+                 FETCH FIRST 1 ROW ONLY)
076900170626     C/END-EXEC
077000180215     c     dopo_sql7     tag
077100180215EL    *---------  da eliminare
077200180215     C*
077300180215     c                   clear                   CNTATB
077400180215     c                   eval      CNTVER  = sVERSION
077500180215     c                   eval      CNTISO2 = oD_ISO2
077600180216     c                   move      oD_BUCN       CNTBUCN
077700180216
077800180215     c     kCNT          chain     DPCNT12i
077900180215     c                   if        %Found(DPCNT12i)
078000180215     C                   eval      oD_NTWC = CNTNTWC
078100180215EL   C****                   if        sqlcod = *zeros
078200170628     C                   else
078300170628     C*
078400170628     C* Quindi aggancio i dati della NAZIONE
078500180215     C
078600180215EL    *---------  da eliminare
078700180215     C                   goto      dopo_sql8
078800170628     C
078900170628     C/EXEC SQL
079000170628     C+ SET :oD_ISON = (SELECT CCCISON FROM DPCCC10F
079100170628     C+                 WHERE CCCATB = ' ' AND CCCVER = :sVERSION AND
079200170628     C+                 CCCISO2 = :oD_ISO2
079300170628     C+                 FETCH FIRST 1 ROW ONLY)
079400170628     C/END-EXEC
079500180215     c     dopo_sql8     tag
079600180215EL    *---------  da eliminare
079700180215     C*
079800180215     C                   eval      cccATB = *blanks
079900180215     C                   eval      cccVER = sVERSION
080000180215     C                   eval      cccISO2= oD_ISO2
080100180216     C     KCCC          chain     dpccc11i
080200180216     C                   if        %found(dpccc11i)
080300180215     c                   eval      oD_ISON = CCCISON
080400180215EL   C*******                   if        sqlcod = *zeros
080500170628     C                   eval      oD_NTWC = %editc(oD_ISON:'X')
080600170628     C                   else
080700170628     C                   eval      wNotFoundCR1 = '1'
080800170626     C                   endif
080900170628     C                   endif
081000170626     C*
081100170626     C                   else
081200170628     C                   eval      wNotFoundCR1 = '1'
081300170626     C                   endif
081400180215     C
081500180215EL    *---------  da eliminare
081600180215     C                   goto      dopo_sql9
081700170626     C/EXEC SQL
081800170626     C+ close C_R1
081900170626     C/END-EXEC
082000180215     c     dopo_sql9     tag
082100180215EL    *---------  da eliminare
082200180215     C*
082300170628     C***                if        wNotFoundCR1 = *on
082400170627     C***                eval      OSID4ERR  = '1'
082500170627     C***                eval      OSID4ERRL = '96'
082600170627     C***                eval      OSID4ERRD = 'Errore in calcolo ' +
082700170627     C***                          'instradamento R1 (DPCR110F)'
082800170627     C***                seton                                        70
082900170627     C***                endif
083000170626     C*
083100180216     C                   ENDSR
083200170626     C*------------------------------------------------------------------------*
083300170626     C* RTVCR2 - Esecuzione ricerca R2
083400170626     C*------------------------------------------------------------------------*
083500170626     C     RTVCR2        BEGSR
083600170626     C*
083700170626     C* Inizializzazioni
083800170626     C                   clear                   oD_DPT
083900170626     C                   clear                   oD_DPTISO2
084000170626     C                   clear                   oD_DSTR
084100170626     C                   clear                   oD_SSORT
084200170626     C                   clear                   oD_PTNC
084300170627     C                   clear                   oD_DPTIATA
084400170626     C                   clear                   stringaSQL
084500170628     C                   clear                   wNotFoundCR2
084600170627     C                   clear                   DS_DCDP
084700170626     C*
084800180215     C
084900180215EL    *---------  da eliminare
085000180215     C                   goto      dopo_sql10
085100180220     C                   clear                   wBUFFER
085200170626     C* Compongo la stringa SQL
085300170626       stringaSQL =
085400170626       'SELECT CR2DDPT, CR2SSORT, CR2PTNC FROM DPCR210F ' +
085500170626       'WHERE CR2ATB=§ § and CR2VER='+%editc(sVERSION:'4')+' and ' +
085600170626       'CR2TRADS=§'+%trim(ISID4TRADS)+'§ and ' +
085700170626       'CR2BUCN='+%editc(oD_BUCN:'X')+' and ' +
085800180212       'CR2ISO2=§'+%trim(oD_ISO2)+'§ and (CR2PTCB = § § or §' +
085900180212       %trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE and ' +
086000180212       'CR2ISO2<>§GB§ or §' +
086100180212       %trim(USID4CAD)+'§ between CR2PTCB and CR2PTCE and ' +
086200180213       'CR2ISO2 =§GB§ and CHARACTER_LENGTH(trim(CR2PTCB))<= ' +
086300180213       'CHARACTER_LENGTH(§' + %trim(USID4CAD) + '§) and ' +
086400180213       'CHARACTER_LENGTH(trim(CR2PTCE))>= ' +
086500180213       'CHARACTER_LENGTH(§' + %trim(USID4CAD) + '§)) and ' +
086600170706       '(CR2NTWP=§ § or CR2NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
086700170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
086800170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
086900170626       ' (CR2ROUT=§ § and CR2ROUPB=§ §) or' +
087000170627       ' (CR2ROUT=§I§ and §'+oO_ICC+'§ between CR2ROUPB and CR2ROUPE) or'+
087100170627       ' (CR2ROUT=§C§ and CR2ROUPB=§'+oO_ISO2+'§) or ' +
087200170627       '(CR2ROUT=§D§ and §'+oO_DPT+'§ between CR2ROUPB and CR2ROUPE) or'+
087300170626       ' (CR2ROUT=§G§ and CR2ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
087400170627       ' (CR2ROUT=§B§ and CR2ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
087500170626       ') ' +
087600170626       'ORDER BY CR2PTY DESC ' +
087700170627       'FETCH FIRST 1 ROW ONLY ';
087800170627
087900170626     C*
088000170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
088100170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
088200170626     C
088300170626     C* Quindi eseguo stringa SQL così ottenuta
088400170626     C*
088500170626     C/EXEC SQL
088600170626     C+ prepare S_R2 from :stringaSQL
088700170626     C/END-EXEC
088800170626     C
088900170626     C/EXEC SQL
089000170626     C+ declare C_R2 asensitive cursor for S_R2
089100170626     C/END-EXEC
089200170626     C
089300170626     C/EXEC SQL
089400170626     C+ open C_R2
089500170626     C/END-EXEC
089600170626     C
089700170626     C/EXEC SQL
089800170626     C+ Fetch C_R2 into :oD_DPT, :oD_SSORT, :oD_PTNC
089900170626     C/END-EXEC
090000180215     c     dopo_sql10    tag
090100180215EL    *---------  da eliminare
090200180215     C*
090300180215     C                   clear                   cr2_found         1
090400180215     C                   clear                   cr2ATB
090500180215     C                   eval       cr2VER   = sVERSION
090600180215     C                   eval       cr2TRADS = ISID4TRADS
090700180215     C                   eval       cr2ISO2  = oD_ISO2
090800180215     C                   eval       cr2BUCN  = oD_BUCN
090900180215      *
091000180215     C*   legge al contrario per la Priorità  DESCEND
091100180215     c     kCR2          setgt     DPCR213i
091200180215     c     kCR2          readpe    DPCR213i
091300180215     c                   dow        not %EoF(DPCR213i)
091400180215     C                   IF        (CR2PTCB =' '  or
091500180215     c                              CR2PTCB <= USID4CAD and CR2PTCE>= USID4CAD
091600180215     c                              and CR2ISO2<>'GB'
091700180215     c                             or
091800180215     c                              CR2PTCB<= USID4CAD and CR2PTCE>= USID4CAD
091900180215     c                              and CR2ISO2 ='GB' and
092000180215     c                             %Len(%Trim(CR2PTCB))<= %Len(%Trim(USID4CAD))
092100180215     c                              and
092200180215     c                             %Len(%Trim(CR2PTCE))>= %Len(%Trim(USID4CAD))
092300180215     c                             )
092400180215     c                             and (
092500180215     c                              (CR2ROUT=' ' and CR2ROUPB=' ') or
092600180215     c                              (CR2ROUT='C' and CR2ROUPB=oO_ISO2) or
092700180215     c                              (CR2ROUT='I' and
092800180215     c                              CR2ROUPB<= oO_ICC and CR2ROUPE>= oO_ICC) or
092900180215     c                              (CR2ROUT='D' and
093000180215     c                              CR2ROUPB<= oO_DPT and CR2ROUPE>= oO_DPT) or
093100180216     c                              (CR2ROUT='B' and
093200180216     c                              CR2ROUPB = %editc(oO_BUCN:'X')) or
093300180215     c                              (CR2ROUT='G' and CR2ROUPB = oO_GRPL)
093400180215     c                                 )
093500180215     C*  o blank
093600180215     c                   if        CR2NTWP = ' '
093700180215     C                   eval       oD_DPT = CR2DDPT
093800180215     C                   eval       oD_SSORT = CR2SSORT
093900180215     C                   eval       oD_PTNC = CR2PTNC
094000180215     C                   eval       cr2_found ='1'
094100180215     c                   leave
094200180215     c                   else
094300180215      *- o verificato
094400180215     c                   eval        CNPATB  = ' '
094500180215     c                   eval        CNPVER  = sVERSION
094600180215     c                   eval        CNPNTWP = CR2NTWP
094700180221     c                   eval        CNPSORCB = ISID4SORC
094800180221     c     kCNP1         setll     dpcnp11i
094900180215     c     kCNP          reade     dpcnp11i
095000180215     c                   dow       not %EoF(dpcnp11i)
095100180215      *-
095200180221     c                   if         CNPSORCE >= ISID4SORC
095300180215     C                   eval       oD_DPT = CR2DDPT
095400180215     C                   eval       oD_SSORT = CR2SSORT
095500180215     C                   eval       oD_PTNC = CR2PTNC
095600180215     C                   eval       cr2_found ='1'
095700180215     c                   leave
095800180215     c                   endif
095900180215      *-
096000180215     c     kCNP          reade     dpcnp11i
096100180215     c                   enddo
096200180215      *-
096300180215      *- se trovato deve uscire dal ciclo principale
096400180215     C                   if         cr2_found ='1'
096500180215     c                   leave
096600180215     c                   endif
096700180215      *-
096800180215     c                   endif
096900180216     c                   endif
097000180215     c     kCR2          readpe    DPCR213i
097100180215     c                   enddo
097200180215     C*
097300180215     C*****                   if        sqlcod = *zeros
097400180215     C                   if         cr2_found ='1'
097500170626     C*
097600170626     C* Quindi aggancio i dati del DESTINATION DEPOT
097700180215     C
097800180215EL    *---------  da eliminare
097900180215     C                   goto      dopo_sql11
098000170626     C
098100170627     C/EXEC SQL
098200170627     C+ SET :wBUFFER  = (SELECT
098300170627     C+               '1' CONCAT CDPISO2 CONCAT CDPDSTR CONCAT CDPIATA
098400170629     C+               CONCAT CDPGRPID
098500170627     C+               FROM DPCDP10F
098600170627     C+               WHERE CDPATB = ' ' AND CDPVER = :sVERSION AND
098700170627     C+               CDPDPTN = :oD_DPT
098800170627     C+               FETCH FIRST 1 ROW ONLY)
098900170626     C/END-EXEC
099000180215     c     dopo_sql11    tag
099100180215EL    *---------  da eliminare
099200180215     C*
099300180215     c                   clear                   cdpATB
099400180215     c                   eval      cdpVER = sVERSION
099500180220     c                   eval      cdpDPTN = oD_DPT
099600180215     c     kCDP          chain     DPCDP11i
099700180215     c                   if        %Found(DPCDP11i)
099800180220     C                   eval      oD_DPTISO2 = cdpISO2
099900180220     C                   eval      oD_DSTR    = cdpDSTR
100000180220     C                   eval      oD_DPTIATA = cdpIATA
100100180220     C                   eval      oD_GRPL    = cdpGRPid
100200170626     C                   else
100300170626     C                   eval      oD_DPTISO2 = oD_ISO2
100400170626     C                   endif
100500170626     C*
100600170626     C                   else
100700170628     C                   eval      wNotFoundCR2 = '1'
100800170626     C                   endif
100900170626     C
101000180215     C
101100180215EL    *---------  da eliminare
101200180215     C                   goto      dopo_sql12
101300170626     C/EXEC SQL
101400170626     C+ close C_R2
101500170626     C/END-EXEC
101600180215     c     dopo_sql12    tag
101700180215EL    *---------  da eliminare
101800180215     C*
101900170628     C                   if        wNotFoundCR2 = *on
102000170626     C                   eval      OSID4ERR  = '1'
102100170626     C                   eval      OSID4ERRL = '96'
102200170626     C                   eval      OSID4ERRD = 'Errore in calcolo ' +
102300170626     C                             'instradamento R2 (DPCR210F)'
102400170626     C                   seton                                        70
102500170626     C                   endif
102600170626     C*
102700170626     C                   ENDSR
102800170626     C*------------------------------------------------------------------------*
102900170626     C* RTVCR3 - Esecuzione ricerca R3
103000170626     C*------------------------------------------------------------------------*
103100170626     C     RTVCR3        BEGSR
103200170626     C*
103300170626     C* Inizializzazioni
103400170626     C                   clear                   oD_OSORT
103500170626     C                   clear                   stringaSQL
103600170628     C                   clear                   wNotFoundCR3
103700170626     C*
103800180215EL    *---------  da eliminare
103900180215     C                   goto      dopo_sql13
104000180215     C
104100170626     C* Compongo la stringa SQL
104200170626       stringaSQL =
104300170626       'SELECT CR3OSORT FROM DPCR310F ' +
104400170626       'WHERE CR3ATB=§ § and CR3VER='+%editc(sVERSION:'4')+' and ' +
104500170626       'CR3ISO2=§'+%trim(oD_ISO2)+'§ and ' +
104600170626       '(CR3DDPT=§ § or CR3DDPT=§'+oD_DPT+'§) and ' +
104700170626       '(CR3PTCB=§ § or ' +
104800170626       '§'+%trim(USID4CAD)+'§ between CR3PTCB and CR3PTCE) and ' +
104900170706       '(CR3NTWP=§ § or CR3NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
105000170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
105100170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
105200170626       ' (CR3ROUT=§ § and CR3ROUPB=§ §) or' +
105300170627       ' (CR3ROUT=§I§ and §'+oO_ICC+'§ between CR3ROUPB and CR3ROUPE) or'+
105400170627       ' (CR3ROUT=§C§ and CR3ROUPB=§'+oO_ISO2+'§) or ' +
105500170627       '(CR3ROUT=§D§ and §'+oO_DPT+'§ between CR3ROUPB and CR3ROUPE) or'+
105600170626       ' (CR3ROUT=§G§ and CR3ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
105700170627       ' (CR3ROUT=§B§ and CR3ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
105800170626       ') ' +
105900170626       'ORDER BY CR3PTY DESC ' +
106000170627       'FETCH FIRST 1 ROW ONLY ';
106100170627
106200170626     C*
106300170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
106400170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
106500170626     C
106600170626     C* Quindi eseguo stringa SQL così ottenuta
106700170626     C*
106800170626     C/EXEC SQL
106900170626     C+ prepare S_R3 from :stringaSQL
107000170626     C/END-EXEC
107100170626     C
107200170626     C/EXEC SQL
107300170626     C+ declare C_R3 asensitive cursor for S_R3
107400170626     C/END-EXEC
107500170626     C
107600170626     C/EXEC SQL
107700170626     C+ open C_R3
107800170626     C/END-EXEC
107900170626     C
108000170626     C/EXEC SQL
108100170626     C+ Fetch C_R3 into :oD_OSORT
108200170626     C/END-EXEC
108300180215     c     dopo_sql13    tag
108400180215EL    *---------  da eliminare
108500180215     C*
108600180215     C                   clear                   cr3_found         1
108700180215     C                   clear                   cr3ATB
108800180215     C                   eval       cr3VER   = sVERSION
108900180215     C                   eval       cr3ISO2  = oD_ISO2
109000180215      *
109100180215     C*   legge al contrario per la Priorità  DESCEND
109200180215     c     kCR3          setgt     DPCR313i
109300180215     c     kCR3          readpe    DPCR313i
109400180215     c                   dow        not %EoF(DPCR313i)
109500180216     C                   IF        (CR3DDPT=' ' or CR3DDPT=oD_DPT) and
109600180216     c                             (CR3PTCB=' ' or
109700180216     c                              CR3PTCB<=USID4CAD and CR3PTCE>=USID4CAD)
109800180215     c                             and (
109900180216     c                              (CR3ROUT=' ' and CR3ROUPB=' ') or
110000180216     c                              (CR3ROUT='C' and CR3ROUPB=oO_ISO2) or
110100180216     c                              (CR3ROUT='I' and
110200180216     c                              CR3ROUPB<= oO_ICC and CR3ROUPE>= oO_ICC) or
110300180216     c                              (CR3ROUT='D' and
110400180216     c                              CR3ROUPB<= oO_DPT and CR3ROUPE>= oO_DPT) or
110500180216     c                              (CR3ROUT='B' and
110600180216     c                              CR3ROUPB = %editc(oO_BUCN:'X')) or
110700180216     c                              (CR3ROUT='G' and CR3ROUPB = oO_GRPL)
110800180215     c                                 )
110900180215     C*  o blank
111000180216     c                   if        CR3NTWP = ' '
111100180216     C                   eval       oD_OSORT = CR3OSORT
111200180216     C                   eval       cr3_found ='1'
111300180215     c                   leave
111400180215     c                   else
111500180215      *- o verificato
111600180215     c                   eval        CNPATB  = ' '
111700180215     c                   eval        CNPVER  = sVERSION
111800180216     c                   eval        CNPNTWP = CR3NTWP
111900180221     c                   eval        CNPSORCB = ISID4SORC
112000180221     c     kCNP1         setll     dpcnp11i
112100180215     c     kCNP          reade     dpcnp11i
112200180215     c                   dow       not %EoF(dpcnp11i)
112300180215      *-
112400180221     c                   if           CNPSORCE >= ISID4SORC
112500180216     C                   eval       oD_OSORT = CR3OSORT
112600180216     C                   eval       cr3_found ='1'
112700180215     c                   leave
112800180215     c                   endif
112900180215      *-
113000180215     c     kCNP          reade     dpcnp11i
113100180215     c                   enddo
113200180215      *-
113300180215      *- se trovato deve uscire dal ciclo principale
113400180216     C                   if         cr3_found ='1'
113500180215     c                   leave
113600180215     c                   endif
113700180215      *-
113800180215     c                   endif
113900180216     c                   endif
114000180216     c     kCR3          readpe    DPCR313i
114100180215     c                   enddo
114200180215     C*
114300180215     C*****                   if        sqlcod = *zeros
114400180216     C                   if         cr3_found ='1'
114500170626     C                   else
114600170628     C                   eval      wNotFoundCR3 = '1'
114700170626     C                   endif
114800170626     C
114900180215     C*
115000180215EL    *---------  da eliminare
115100180215     C                   goto      dopo_sql14
115200180215     C
115300170626     C/EXEC SQL
115400170626     C+ close C_R3
115500170626     C/END-EXEC
115600180216     C*
115700180215     c     dopo_sql14    tag
115800180215EL    *---------  da eliminare
115900170626     C*
116000170626     C                   ENDSR
116100170627     C*------------------------------------------------------------------------*
116200170627     C* CHKALW - Verifica instradamenti consentiti ALLOW
116300170627     C*------------------------------------------------------------------------*
116400170627     C     CHKALW        BEGSR
116500170627     C*
116600170627     C* Inizializzazioni
116700170627     C                   clear                   oALLOWID
116800170627     C                   clear                   stringaSQL
116900180219     C                   clear                   cws_found         1
117000170627     C                   clear                   wAllow
117100180216     C*
117200180216EL    *---------  da eliminare
117300180223     C********           goto      dopo_sql15
117400180216     C*
117500170627     C* Compongo la stringa SQL
117600170627       stringaSQL =
117700170627       'SELECT CWSALWID FROM DPCWS10F ' +
117800170627       'WHERE CWSATB=§ § and CWSVER='+%editc(sVERSION:'4')+' and (' +
117900170629       '(CWSRULFT=§I§ and §'+oO_ICC+'§ between CWSRULFB and CWSRULFE) or ' +
118000170724       // '(CWSRULFT=§N§ and CWSRULFB=§'+oO_GRPL+'§) or ' +     //
118100170629       '(CWSRULFT=§C§ and CWSRULFB=§'+oO_ISO2+'§) or ' +
118200170629       '(CWSRULFT=§D§ and §'+oO_DPT+'§ between CWSRULFB and CWSRULFE) or ' +
118300170629       '(CWSRULFT=§G§ and CWSRULFB in (§'+%trim(oO_GRPL)+'§)) or ' +
118400170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
118500170629       '(CWSRULFT=§B§ and CWSRULFB=§'+%editc(oO_BUCN:'X')+'§)) and (' +
118600170629       %editc(ISID4SORC:'X')+' between CWSRULSB and CWSRULSE or ' +
118700170629       'CWSRULSB=0) and (' +
118800170629       '(CWSRULTT=§D§ and §'+oD_DPT+'§ between CWSRULTB and CWSRULTE) or ' +
118900170629       '(CWSRULTT=§C§ and CWSRULTB=§'+oD_DPTISO2+'§) or ' +
119000170629       '(CWSRULTT=§N§ and CWSRULTB=§'+oD_NTWC+'§) or ' +
119100170629       '(CWSRULTT=§B§ and CWSRULTB=§'+%editc(oD_BUCN:'X')+oD_DPTISO2+'§) or ' +
119200170724       '(CWSRULTT=§G§ and CWSRULTB in (§'+%trim(oD_GRPL)+'§)) ) and (' +
119300170724       '(§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWSZONTB and CWSZONTE or ' +
119400170724       '(CWSZONTB=§ § and CWSZONTE=§ §))) ' +
119500170629
119600170627       'FETCH FIRST 1 ROW ONLY ';
119700170627     C*
119800170627     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
119900170627     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
120000170627     C
120100170627     C* Quindi eseguo stringa SQL così ottenuta
120200170627     C*
120300170627     C/EXEC SQL
120400170627     C+ prepare S_WS from :stringaSQL
120500170627     C/END-EXEC
120600170627     C
120700170627     C/EXEC SQL
120800170627     C+ declare C_WS asensitive cursor for S_WS
120900170627     C/END-EXEC
121000170627     C
121100170627     C/EXEC SQL
121200170627     C+ open C_WS
121300170627     C/END-EXEC
121400170627     C
121500170627     C/EXEC SQL
121600170627     C+ Fetch C_WS into :oALLOWID
121700170627     C/END-EXEC
121800180223     C                   if        sqlcod = *zeros
121900180223     C                   eval      oALLOWID = CWSALWID
122000180223     C                   eval      cws_found = '1'
122100180223     C                   eval      wAllow = '1'
122200180223     C                   endif
122300180216     C*
122400180223     c**** dopo_sql15    tag
122500180216EL    *---------  da eliminare
122600180223EL    *---------  da eliminare
122700180223     C                   goto      dopo_sql16
122800180216     C*
122900180216     c                   clear                   cwsATB
123000180216     c                   eval      cwsVER = sVERSION
123100180220     c     kCWS          setll     DPCWS11i
123200180220     c     kCWS          reade     DPCWS11i
123300180220     c                   dow       not %EoF(DPCWS11i)
123400180216     C*****                   if        sqlcod = *zeros
123500180220      *
123600180216     c                   if        (    (CWSRULFT='C' and CWSRULFB=oO_ISO2) or
123700180216     c                                  (CWSRULFT='I' and
123800180216     c                             CWSRULFB <=oO_ICC and CWSRULFE >=oO_ICC) or
123900180216     c                                  (CWSRULFT='D' and
124000180216     c                             CWSRULFB <=oO_DPT and CWSRULFE >=oO_DPT) or
124100180216     c                                  (CWSRULFT='G' and
124200180216     c                             CWSRULFB = %trim(oO_GRPL) )  or
124300180216     c                                  (CWSRULFT='B' and CWSRULFB =
124400180216     c                             %editc(oO_BUCN:'X') + oO_ISO2) or
124500180216     c                                  (CWSRULFT='B' and CWSRULFB =
124600180216     c                             %editc(oO_BUCN:'X'))
124700180216     c                             ) and
124800180216     c                             ( CWSRULSB = 0 or
124900180216     c                               CWSRULSB <=  ISID4SORC and
125000180216     c                               CWSRULSE >=  ISID4SORC
125100180216     c                             ) and
125200180216     c                             (    (CWSRULTT='D' and
125300180216     c                             CWSRULTB<=oD_DPT and CWSRULTE>=oD_DPT ) or
125400180216     c                             (CWSRULTT='C' and CWSRULTB=oD_DPTISO2)  or
125500180216     c                             (CWSRULTT='N' and CWSRULTB=oD_NTWC) or
125600180216     c                             (CWSRULTT='B' and CWSRULTB=
125700180216     c                             %editc(oD_BUCN:'X') + oD_DPTISO2) or
125800180216     c                             (CWSRULTT='G' and CWSRULTB=
125900180216     c                             (%trim(oD_GRPL)))
126000180216     c                             ) and
126100180216     c                             (  CWSZONTB <=
126200180216     c                                oD_DPTISO2 + %trim(USID4CAD) and
126300180216     c                                CWSZONTE >=
126400180216     c                                oD_DPTISO2 + %trim(USID4CAD) or
126500180216     c                              (CWSZONTB=' ' and CWSZONTE=' ')
126600180216     c                             )
126700180216     C                   eval      oALLOWID = CWSALWID
126800180219     C                   eval      cws_found = '1'
126900170627     C                   eval      wAllow = '1'
127000180220     C                   leave
127100170627     C                   endif
127200180220     C*
127300180220     c     kCWS          reade     DPCWS11i
127400180220     C                   enddo
127500180223     c     dopo_sql16    tag
127600180223EL    *---------  da eliminare
127700180223     C*
127800170627     C/EXEC SQL
127900170627     C+ close C_WS
128000170627     C/END-EXEC
128100170627     C                   if        wAllow = *on
128200170629     C*
128300170629     C* Se richiesto anche un ADDITIONAL SERVICE CODE => verifico se ALLOW
128400170629     C                   if        ISID4ASCO <> *blanks
128500170629     C                   exsr      CHKCWA
128600170629     C                   endif
128700170627     C                   else
128800170627     C                   eval      OSID4ERR  = '1'
128900170627     C                   eval      OSID4ERRL = '97'
129000170627     C                   eval      OSID4ERRD = 'Instradamento non ' +
129100170627     C                             'consentito. (DPCWS10F)'
129200170627     C                   seton                                        70
129300170627     C                   endif
129400170627     C*
129500170627     C                   ENDSR
129600170629     C*------------------------------------------------------------------------*
129700170629     C* CHKCWA - Verifica instradamenti consentiti ALLOW - ASCODE
129800170629     C*------------------------------------------------------------------------*
129900170629     C     CHKCWA        BEGSR
130000170629     C*
130100170629     C* Inizializzazioni
130200170629     C                   clear                   stringaSQL
130300170629     C                   clear                   wFound
130400180219     C                   clear                   cwa_found         1
130500180219     C*
130600180219EL    *---------  da eliminare
130700180219     C                   goto      dopo_sql17
130800170629     C*
130900170629     C* Compongo la stringa SQL
131000170629       stringaSQL =
131100170629       'SELECT §1§ FROM DPCWA10F ' +
131200170629       'WHERE CWAATB=§ § and CWAVER='+%editc(sVERSION:'4')+' and ' +
131300170629       'CWAALWID='+%editc(oALLOWID:'4')+' and (§' +
131400170629       ISID4ASCO+'§ between CWARULSB and CWARULSE or CWARULSB=§ §) and (' +
131500170629       '§'+oD_DPTISO2+%trim(USID4CAD)+'§ between CWAZONTB and CWAZONTE)) ' +
131600170629       'FETCH FIRST 1 ROW ONLY ';
131700170629     C*
131800170629     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
131900170629     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
132000170629     C
132100170629     C* Quindi eseguo stringa SQL così ottenuta
132200170629     C*
132300170629     C/EXEC SQL
132400170629     C+ prepare S_WA from :stringaSQL
132500170629     C/END-EXEC
132600170629     C
132700170629     C/EXEC SQL
132800170629     C+ declare C_WA asensitive cursor for S_WA
132900170629     C/END-EXEC
133000170629     C
133100170629     C/EXEC SQL
133200170629     C+ open C_WA
133300170629     C/END-EXEC
133400170629     C
133500170629     C/EXEC SQL
133600170629     C+ Fetch C_WS into :wFound
133700170629     C/END-EXEC
133800180219     c     dopo_sql17    tag
133900180219EL    *---------  da eliminare
134000180219     C*
134100180219     c                   eval        CWAATB  = ' '
134200180219     c                   eval        CWAVER  = sVERSION
134300180219     c                   eval        CWAALWID = oALLOWID
134400180219     c     kCWA          setll     dpcwa11i
134500180219     c     kCWA          reade     dpcwa11i
134600180219     c                   dow       not %EoF(dpcwa11i)
134700180219      *-
134800180219     c                   if         CWARULSB <= ISID4ASCO
134900180219     c                               and
135000180219     c                              CWARULSE >= ISID4ASCO
135100180219     c                               or
135200180219     c                              CWARULSB=' '
135300180219     c                               and
135400180219     c                              CWAZONTB <= oD_DPTISO2 + %trim(USID4CAD)
135500180219     c                               and
135600180219     c                              CWAZONTE >= oD_DPTISO2 + %trim(USID4CAD)
135700180219     C                   eval       wfound ='1'
135800180219     C                   eval       cwa_found ='1'
135900180219     c                   leave
136000180219     c                   endif
136100180219      *-
136200180219     c     kCNP          reade     dpcnp11i
136300180219     c                   enddo
136400180219      *-
136500170629     C                   if        wFound = *on
136600170629     C                   else
136700170629     C                   eval      OSID4ERR  = '1'
136800170629     C                   eval      OSID4ERRL = '97'
136900170629     C                   eval      OSID4ERRD = 'Instradamento non ' +
137000170629     C                             'consentito. (DPCWA10F)'
137100170629     C                   seton                                        70
137200170629     C                   endif
137300170629     C
137400180219     C*
137500180219EL    *---------  da eliminare
137600180219     C                   goto      dopo_sql18
137700180219     C*
137800170629     C/EXEC SQL
137900170629     C+ close C_WS
138000170629     C/END-EXEC
138100180219     C*
138200180219     c     dopo_sql18    tag
138300180219EL    *---------  da eliminare
138400170629     C
138500170629     C*
138600170629     C                   ENDSR
138700170626     C*------------------------------------------------------------------------*
138800170626     C* RTVCR4 - Esecuzione ricerca R4
138900170626     C*------------------------------------------------------------------------*
139000170626     C     RTVCR4        BEGSR
139100170626     C*
139200170626     C* Inizializzazioni
139300170626     C                   clear                   oD_CSORT
139400170626     C                   clear                   stringaSQL
139500170628     C                   clear                   wNotFoundCR4
139600180219     C                   clear                   cr4_found         1
139700180219     C*
139800180219EL    *---------  da eliminare
139900180219     C                   goto      dopo_sql19
140000170626     C*
140100170626     C* Compongo la stringa SQL
140200170626       stringaSQL =
140300170627       'SELECT CR4CSORT FROM DPCR410F ' +
140400170626       'WHERE CR4ATB=§ § and CR4VER='+%editc(sVERSION:'4')+' and ' +
140500170626       'CR4ISO2=§'+%trim(oD_ISO2)+'§ and ' +
140600170626       '(CR4DDPT=§ § or CR4DDPT=§'+oD_DPT+'§) and ' +
140700170706       '(CR4NTWP=§ § or CR4NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
140800170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
140900170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
141000170626       ' (CR4ROUT=§ § and CR4ROUPB=§ §) or' +
141100170627       ' (CR4ROUT=§I§ and §'+oO_ICC+'§ between CR4ROUPB and CR4ROUPE) or'+
141200170627       ' (CR4ROUT=§C§ and CR4ROUPB=§'+oO_ISO2+'§) or ' +
141300170627       '(CR4ROUT=§D§ and §'+oO_DPT+'§ between CR4ROUPB and CR4ROUPE) or'+
141400170626       ' (CR4ROUT=§G§ and CR4ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
141500170629       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
141600170627       ' (CR4ROUT=§B§ and CR4ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
141700170626       ') ' +
141800170626       'ORDER BY CR4PTY DESC ' +
141900170627       'FETCH FIRST 1 ROW ONLY ';
142000170627
142100170626     C*
142200170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
142300170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
142400170626     C
142500170626     C* Quindi eseguo stringa SQL così ottenuta
142600170626     C*
142700170626     C/EXEC SQL
142800170626     C+ prepare S_R4 from :stringaSQL
142900170626     C/END-EXEC
143000170626     C
143100170626     C/EXEC SQL
143200170626     C+ declare C_R4 asensitive cursor for S_R4
143300170626     C/END-EXEC
143400170626     C
143500170626     C/EXEC SQL
143600170626     C+ open C_R4
143700170626     C/END-EXEC
143800170626     C
143900170626     C/EXEC SQL
144000170626     C+ Fetch C_R4 into :oD_CSORT
144100170626     C/END-EXEC
144200180219     C*
144300180219     c     dopo_sql19    tag
144400180219EL    *---------  da eliminare
144500180219     C*
144600180219     C                   clear                   cr4ATB
144700180219     C                   eval       cr4VER   = sVERSION
144800180219     C                   eval       cr4ISO2  = oD_ISO2
144900180219      *
145000180219     C*   legge al contrario per la Priorità  DESCEND
145100180219     c     kCR4          setgt     DPCR413i
145200180219     c     kCR4          readpe    DPCR413i
145300180219     c                   dow        not %EoF(DPCR413i)
145400180219     C                   IF        (CR4DDPT=' ' or CR4DDPT=oD_DPT)
145500180219     c                             and (
145600180219     c                              (CR4ROUT=' ' and CR4ROUPB=' ') or
145700180219     c                              (CR4ROUT='C' and CR4ROUPB=oO_ISO2) or
145800180219     c                              (CR4ROUT='I' and
145900180219     c                              CR4ROUPB<= oO_ICC and CR4ROUPE>= oO_ICC) or
146000180219     c                              (CR4ROUT='D' and
146100180219     c                              CR4ROUPB<= oO_DPT and CR4ROUPE>= oO_DPT) or
146200180219     c                              (CR4ROUT='B' and
146300180219     c                              CR4ROUPB = %editc(oO_BUCN:'X')+oO_ISO2) or
146400180219     c                              (CR4ROUT='B' and
146500180219     c                              CR4ROUPB = %editc(oO_BUCN:'X')) or
146600180219     c                              (CR4ROUT='G' and CR4ROUPB = oO_GRPL)
146700180219     c                                 )
146800180219     C*  o blank
146900180219     c                   if        CR4NTWP = ' '
147000180219     C                   eval       oD_CSORT = CR4CSORT
147100180219     C                   eval       cr4_found ='1'
147200180219     c                   leave
147300180219     c                   else
147400180219      *- o verificato
147500180219     c                   eval        CNPATB  = ' '
147600180219     c                   eval        CNPVER  = sVERSION
147700180219     c                   eval        CNPNTWP = CR4NTWP
147800180221     c                   eval        CNPSORCB = ISID4SORC
147900180221     c     kCNP1         setll     dpcnp11i
148000180219     c     kCNP          reade     dpcnp11i
148100180219     c                   dow       not %EoF(dpcnp11i)
148200180219      *-
148300180221     c                   if           CNPSORCE >= ISID4SORC
148400180219     C                   eval       oD_CSORT = CR4CSORT
148500180219     C                   eval       cr4_found ='1'
148600180219     c                   leave
148700180219     c                   endif
148800180219      *-
148900180219     c     kCNP          reade     dpcnp11i
149000180219     c                   enddo
149100180219      *-
149200180219      *- se trovato deve uscire dal ciclo principale
149300180219     C                   if         cr4_found ='1'
149400180219     c                   leave
149500180219     c                   endif
149600180219      *-
149700180219     c                   endif
149800180219     c                   endif
149900180219     c     kCR4          readpe    DPCR413i
150000180219     c                   enddo
150100180219     C*
150200180219     C*****                   if        sqlcod = *zeros
150300180219     C                   if         cr4_found ='1'
150400180219     C                   else
150500180219     C                   eval      wNotFoundCR4 = '1'
150600180219     C                   endif
150700180219     C*
150800180219EL    *---------  da eliminare
150900180219     C                   goto      dopo_sql20
151000180219     C*
151100170626     C/EXEC SQL
151200170626     C+ close C_R4
151300170626     C/END-EXEC
151400180219     C*
151500180219     c     dopo_sql20    tag
151600180219EL    *---------  da eliminare
151700170626     C*
151800170626     C                   ENDSR
151900170626     C*------------------------------------------------------------------------*
152000170626     C* RTVCR5 - Esecuzione ricerca R5
152100170626     C*------------------------------------------------------------------------*
152200170626     C     RTVCR5        BEGSR
152300170626     C*
152400170626     C* Inizializzazioni
152500170626     C                   clear                   oD_DSORT
152600170626     C                   clear                   stringaSQL
152700170628     C                   clear                   wNotFoundCR5
152800180219     C                   clear                   cr5_found         1
152900180219     C*
153000180219EL    *---------  da eliminare
153100180219     C                   goto      dopo_sql21
153200180219     C*
153300170626     C* Compongo la stringa SQL
153400170626       stringaSQL =
153500170627       'SELECT CR5DSORT FROM DPCR510F ' +
153600170626       'WHERE CR5ATB=§ § and CR5VER='+%editc(sVERSION:'4')+' and ' +
153700170724       'CR5ISO2=§'+%trim(oD_ISO2)+'§ and ' +
153800170627       'CR5DDPT=§'+oD_DPT+'§ and ' +
153900170626       '(CR5PTCB=§ § or ' +
154000170626       '§'+%trim(USID4CAD)+'§ between CR5PTCB and CR5PTCE) and ' +
154100170706       '(CR5NTWP=§ § or CR5NTWP in (SELECT CNPNTWP FROM DPCNP10F WHERE ' +
154200170706       'CNPATB=§ § and CNPVER='+%editc(sVERSION:'4')+' and §' +
154300170626       %editc(ISID4SORC:'X') +'§ between CNPSORCB and CNPSORCE)) and (' +
154400170626       ' (CR5ROUT=§ § and CR5ROUPB=§ §) or' +
154500170627       ' (CR5ROUT=§I§ and §'+oO_ICC+'§ between CR5ROUPB and CR5ROUPE) or'+
154600170627       ' (CR5ROUT=§C§ and CR5ROUPB=§'+oO_ISO2+'§) or ' +
154700170627       '(CR5ROUT=§D§ and §'+oO_DPT+'§ between CR5ROUPB and CR5ROUPE) or'+
154800170626       ' (CR5ROUT=§G§ and CR5ROUPB in (§'+%trim(oO_GRPL)+'§)) or' +
154900170629       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+oO_ISO2+'§) or ' +
155000170627       ' (CR5ROUT=§B§ and CR5ROUPB=§'+%editc(oO_BUCN:'X')+'§) ' +
155100170626       ') ' +
155200170626       'ORDER BY CR5PTY DESC ' +
155300170627       'FETCH FIRST 1 ROW ONLY ';
155400170626     C*
155500170626     C* Sostituisco inserendo gli apici laddove necessario secondo sintassi SQL
155600170626     C                   eval      stringaSQL = %scanrpl('§':api:stringaSQL)
155700170626     C
155800170626     C* Quindi eseguo stringa SQL così ottenuta
155900170626     C*
156000170626     C/EXEC SQL
156100170626     C+ prepare S_R5 from :stringaSQL
156200170626     C/END-EXEC
156300170626     C
156400170626     C/EXEC SQL
156500170626     C+ declare C_R5 asensitive cursor for S_R5
156600170626     C/END-EXEC
156700170626     C
156800170626     C/EXEC SQL
156900170626     C+ open C_R5
157000170626     C/END-EXEC
157100170626     C
157200170626     C/EXEC SQL
157300170626     C+ Fetch C_R5 into :oD_DSORT
157400170626     C/END-EXEC
157500180219     C*
157600180219     c     dopo_sql21    tag
157700180219EL    *---------  da eliminare
157800180219     C*
157900180219     C                   clear                   cr5ATB
158000180219     C                   eval       cr5VER  = sVERSION
158100180219     C                   eval       CR5DDPT = oD_DPT
158200180219      *
158300180219     C*   legge al contrario per la Priorità  DESCEND
158400180219     c     kCR5          setgt     DPCR513i
158500180219     c     kCR5          readpe    DPCR513i
158600180219     c                   dow        not %EoF(DPCR513i)
158700180219     C                   IF        (CR5ISO2 = oD_ISO2)  and
158800180219     c                             (CR5PTCB=' ' or
158900180220     c                             CR5PTCB <=USID4CAD and CR5PTCE >=USID4CAD)
159000180219     c                             and (
159100180219     c                              (CR5ROUT=' ' and CR5ROUPB=' ') or
159200180219     c                              (CR5ROUT='C' and CR5ROUPB=oO_ISO2) or
159300180219     c                              (CR5ROUT='I' and
159400180219     c                               CR5ROUPB <= oO_ICC and CR5ROUPE >= oO_ICC)
159500180219     C                               or
159600180219     c                              (CR5ROUT='D' and
159700180219     c                              CR5ROUPB <= oO_DPT and CR5ROUPE >= oO_DPT)
159800180219     C                               or
159900180219     c                              (CR5ROUT='B' and
160000180219     c                              CR5ROUPB = %editc(oO_BUCN:'X')+ oO_ISO2) or
160100180219     c                              (CR5ROUT='B' and
160200180219     c                              CR5ROUPB = %editc(oO_BUCN:'X')) or
160300180219     c                              (CR5ROUT='G' and CR5ROUPB = oO_GRPL)
160400180219     c                                 )
160500180219     C*  o blank
160600180219     c                   if        CR4NTWP = ' '
160700180219     C                   eval       oD_DSORT = CR5DSORT
160800180219     C                   eval       cr5_found ='1'
160900180219     c                   leave
161000180219     c                   else
161100180219      *- o verificato
161200180219     c                   eval        CNPATB  = ' '
161300180219     c                   eval        CNPVER  = sVERSION
161400180219     c                   eval        CNPNTWP = CR5NTWP
161500180221     c                   eval        CNPSORCB = ISID4SORC
161600180221     c     kCNP1         setll     dpcnp11i
161700180219     c     kCNP          reade     dpcnp11i
161800180219     c                   dow       not %EoF(dpcnp11i)
161900180219      *-
162000180221     c                   if           CNPSORCE >= ISID4SORC
162100180219     C                   eval       oD_DSORT = CR5DSORT
162200180219     C                   eval       cr5_found ='1'
162300180219     c                   leave
162400180219     c                   endif
162500180219      *-
162600180219     c     kCNP          reade     dpcnp11i
162700180219     c                   enddo
162800180219      *-
162900180219      *- se trovato deve uscire dal ciclo principale
163000180219     C                   if         cr5_found ='1'
163100180219     c                   leave
163200180219     c                   endif
163300180219      *-
163400180219     c                   endif
163500180219     c                   endif
163600180219     c     kCR5          readpe    DPCR513i
163700180219     c                   enddo
163800180219     C*
163900180219     C*****                   if        sqlcod = *zeros
164000180219     C                   if         cr5_found ='1'
164100180219     C                   else
164200180219     C                   eval      wNotFoundCR5 = '1'
164300180219     C                   endif
164400180219     C*
164500180219EL    *---------  da eliminare
164600180219     C                   goto      dopo_sql22
164700180219     C*
164800170626     C/EXEC SQL
164900170626     C+ close C_R5
165000170626     C/END-EXEC
165100180219     C*
165200180219     c     dopo_sql22    tag
165300180219EL    *---------  da eliminare
165400180219     C*
165500170626     C                   ENDSR
165600170627     C*------------------------------------------------------------------------*
165700170627     C* EXEOUT - Valorizzo paramteri di output
165800170627     C*------------------------------------------------------------------------*
165900170627     C     EXEOUT        BEGSR
166000170627     C*
166100170627     C                   eval      OSID4VER   = sVERSION
166200170627     C                   eval      OSID4VERD  = oVER_DPD
166300170627     C                   eval      OSID4VDDE  = CVEDDE
166400170627     C                   eval      OSID4DBUCN = oD_BUCN
166500170627     C                   eval      OSID4DBUCA = oD_BUCA
166600170627     C                   eval      OSID4DNTWC = oD_NTWC
166700170627     C                   eval      OSID4DDPTC = oD_DPTISO2
166800170627     C                   eval      OSID4DDPT  = oD_DPT
166900170627     C                   eval      OSID4DSTR  = oD_DSTR
167000170627     C                   eval      OSID4SSORT = oD_SSORT
167100170627     C                   eval      OSID4DPTNC = oD_PTNC
167200170627     C                   eval      OSID4OSORT = oD_OSORT
167300170627     C                   eval      OSID4CSORT = oD_CSORT
167400170627     C                   eval      OSID4DSORT = oD_DSORT
167500170627     C                   eval      OSID4BCID  = oVER_BCID
167600170627     C                   eval      OSID4SRVX  = oSRV_TXT
167700170627     C                   eval      OSID4SRVM  = oSRV_MRK
167800170627     C                   eval      OSID4ODPT  = oO_DPT
167900170627     C                   eval      OSID4ODPTC = oO_ISO2
168000170627     C                   eval      OSID4OBUCN = oO_BUCN
168100170627     C                   eval      OSID4OGRPL = oO_GRPL
168200170627     C                   eval      OSID4OICC  = oO_ICC
168300170627     C                   eval      OSID4ALWID = oALLOWID
168400170627     C                   eval      OSID4IATA  = oD_DPTIATA
168500170629     C                   evalr     OSID4CADP  = %trim(USID4CAD)
168600170629     C                   eval      OSID4CADP = %scanrpl(' ':'0':OSID4CADP)
168700170627     C*
168800170627     C* Sancisco esito a OK
168900170627     C                   movel     *blanks       OSID4ERR
169000170627     C*
169100170627     C                   ENDSR
169200170626
169300170626
169400170626     C*------------------------------------------------------------------------*
169500170626     C* CARTAB - Caricamemto tabelle si procedura
169600170626     C*------------------------------------------------------------------------*
169700170626     C     CARTAB        BEGSR
169800170626     C*
169900170626     C* Reperisco i valori tabellati di procedura
170000170626     C                   eval      tblKUT = 1
170100170626     C                   eval      tblCOD = '3I'
170200170626     C                   eval      tblKEY ='DPD'
170300170626     C     KEYtab00_C    chain     tabel00f
170400170626     C                   if        %found(tabel00f)
170500170626     C                   eval      DS3IDP = tblUNI
170600170627     C                   eval      gBU   = §3IBUCN
170700170627     C                   eval      gISO2 = §3IISO2
170800170626     C                   else
170900170626     C                   eval      OSID4ERR  = '1'
171000170627     C                   eval      OSID4ERRL = '80'
171100170627     C                   eval      OSID4ERRD = 'Tabella 3I valore §3IBUC non ' +
171200170626     C                             'trovato'
171300170626     C                   seton                                        70
171400170626     C                   endif
171500170626     C*
171600170626     C                   ENDSR
171700170620
171800170620
171900170620     C*--------------------------------------------------------------------------------------------*
172000060515     C* *inzsr - operazioni iniziali
172100060515     C*--------------------------------------------------------------------------------------------*
172200000000     C     *inzsr        BEGSR
172300060515     C*
172400060515     C* Ricevimento parametri
172500060515     C     *ENTRY        PLIST
172600170627     C                   PARM                    TISID4DS
172700060510     C*
172800060510     C* CALCOLA LA DATA CORRENTE
172900170620     C                   Z-ADD     *zeros        datcor            8 0
173000170620     C                   EVAL      datcor = %dec(%date() : *iso)
173100170626     C*
173200170626     C* CARICAMENTO TABELLE DI PROCEDURA
173300170626     C                   EXSR      CARTAB
173400170626     C*
173500170626     C* TABEL00F - Completa
173600170626     C     KEYtab00_C    KLIST
173700170626     C                   KFLD                    tblKUT
173800170626     C                   KFLD                    tblCOD
173900170626     C                   KFLD                    tblKEY
174000170622     C*
174100170622     C* DPCCC14I - Completa
174200170622     C     KEYccc14_C    KLIST
174300170622     C                   KFLD                    cccATB
174400170622     C                   KFLD                    cccVER
174500170622     C                   KFLD                    cccBRT
174600180215     C*
174700180216     C* DPCCC11I - Completa
174800180215     C     KCCC          KLIST
174900180215     C                   KFLD                    cccATB
175000180215     C                   KFLD                    cccVER
175100180215     C                   KFLD                    cccISO2
175200060515     C*
175300180215     c     kCPCset       Klist
175400180215     C                   KFLD                    cpcATB
175500180215     C                   KFLD                    cpcVER
175600180215     C                   KFLD                    cpcISO2
175700180215     C                   KFLD                    cpcPTCB
175800180215     C*
175900180215     c     kCPC          Klist
176000180215     C                   KFLD                    cpcATB
176100180215     C                   KFLD                    cpcVER
176200180215     C                   KFLD                    cpcISO2
176300180216     C*
176400180216     c     kCdp          Klist
176500180216     C                   KFLD                    cdpATB
176600180216     C                   KFLD                    cdpVER
176700180216     C                   KFLD                    cdpDPTN
176800180216     C*
176900180216
177000180216     c     kCbu          Klist
177100180216     C                   KFLD                    cbuATB
177200180216     C                   KFLD                    cbuVER
177300180216     C                   KFLD                    cbuBUCN
177400180216     C*
177500180216     c     kCSO          Klist
177600180215     C                   KFLD                    csoATB
177700180215     C                   KFLD                    csoVER
177800180215     C                   KFLD                    csoSORC
177900180215     C*
178000180215     c     kCIC          Klist
178100180215     C                   KFLD                    cicATB
178200180215     C                   KFLD                    cicVER
178300180215     C                   KFLD                    cicICC
178400180215     C*
178500180219     c     kCWA          Klist
178600180219     C                   KFLD                    cwaATB
178700180219     C                   KFLD                    cwaVER
178800180219     C                   KFLD                    cwaALWID
178900180219     C*
179000180215     c     kCR1          Klist
179100180215     C                   KFLD                    cr1ATB
179200180215     C                   KFLD                    cr1VER
179300180215     C                   KFLD                    cr1TRADS
179400180215     C                   KFLD                    cr1ISO2
179500180215     C*
179600180215     c     kCR2          Klist
179700180215     C                   KFLD                    cr2ATB
179800180215     C                   KFLD                    cr2VER
179900180215     C                   KFLD                    cr2TRADS
180000180215     C                   KFLD                    cr2BUCN
180100180215     C                   KFLD                    cr2ISO2
180200180215     C*
180300180215     c     kCNP          Klist
180400180215     C                   KFLD                    cnpATB
180500180215     C                   KFLD                    cnpVER
180600180215     C                   KFLD                    cnpNTWP
180700180221     C*
180800180221     c     kCNP1         Klist
180900180221     C                   KFLD                    cnpATB
181000180221     C                   KFLD                    cnpVER
181100180221     C                   KFLD                    cnpNTWP
181200180221     C                   KFLD                    CNPSORCB
181300180221     C*
181400180215     c     kCNT          Klist
181500180215     C                   KFLD                    cntATB
181600180215     C                   KFLD                    cntVER
181700180215     C                   KFLD                    cntISO2
181800180215     C                   KFLD                    cntBUCN
181900180215     C*
182000180215     c     kCR3          Klist
182100180215     C                   KFLD                    cr3ATB
182200180215     C                   KFLD                    cr3VER
182300180215     C                   KFLD                    cr3ISO2
182400180219     C*
182500180219     c     kCR4          Klist
182600180219     C                   KFLD                    cr4ATB
182700180219     C                   KFLD                    cr4VER
182800180219     C                   KFLD                    cr4ISO2
182900180216     C*
183000180219     c     kCR5          Klist
183100180219     C                   KFLD                    cr5ATB
183200180219     C                   KFLD                    cr5VER
183300180219     C                   KFLD                    CR5DDPT
183400180219     C*
183500180216     c     kCWS          Klist
183600180216     C                   KFLD                    CWSATB
183700180216     C                   KFLD                    CWSVER
183800180215     C*
183900060515     C                   ENDSR
