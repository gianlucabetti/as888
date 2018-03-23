000100020806     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300161201     H* FATTURAZIONE INFRAGRUPPO MCUBI - KG e COLLI
000400000000     H* NOTE: Compilare il programma con gli OVRPRTF del clp di guida
000500000000     F*------------------------------------------------------------------------*
000600161221     FTABEL00F  UF A E           K DISK
000700161214     FAZORG01L  IF   E           K DISK
000800161214     F* 3 file di work
000900161219     FTNFIG00f  O  A E           K DISK    usropn
001000100107     F* RIEPILOGO
001100161219     FTREC28RP  O    E             PRINTER OFLIND(*IN11) usropn
001200161214     F                                     RENAME(EC27TE:RIEC27TE)
001300161214     F                                     RENAME(EC27SO:RIEC27SO)
001400161214     F                                     RENAME(EC27IN:RIEC27IN)
001500161214     F                                     RENAME(EC27DE:RIEC27DE)
001600161214     F                                     RENAME(EC27TO:RIEC27TO)
001700161214     F                                     RENAME(EC27FI:RIEC27FI)
001800990420     F* DETTAGLIO
001900161219     FTREC28DP  O    E             PRINTER OFLIND(*IN01) usropn
002000161214     F                                     RENAME(EC28TE:DEEC28TE)
002100161214     F                                     RENAME(EC28SO:DEEC28SO)
002200161214     F                                     RENAME(EC28I1:DEEC28I1)
002300161214     F                                     RENAME(EC28D1:DEEC28D1)
002400161214     F                                     RENAME(EC28I2:DEEC28I2)
002500161214     F                                     RENAME(EC28D2:DEEC28D2)
002600161214     F                                     RENAME(EC28I3:DEEC28I3)
002700161214     F                                     RENAME(EC28D3:DEEC28D3)
002800161214     F                                     RENAME(EC28SE:DEEC28SE)
002900161214     F                                     RENAME(EC28TO:DEEC28TO)
003000161214     F                                     RENAME(EC28FI:DEEC28FI)
003100161219     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) usropn
003200000000     D*------------------------------------------------------------------------*
003300161220      **  ABILITA LE STAMPE O LE ESCLUDE
003400161220     D*------------------------------------------------------------------------*
003500161220      *
003600161220     d stampa_dett     s              1    inz('S')
003700161220     d stampa_riep     s              1    inz('N')
003800161220      *
003900161220     d Stampa_TGEN     s              1    inz('N')
004000000000     D*------------------------------------------------------------------------*
004100170104     D WrkSqlCmd       S           8192
004200000000     D*---
004300000000     D* DECODIFICA RIGHE IN TESTATA
004400161220     D STM             S             66    DIM(15) CTDATA PERRCD(1)
004500000000     D*---
004600000000     D* ORGANIGRAMMA
004700000000     D ORG             S              3  0 DIM(500)
004800000000     D ODES            S             20    DIM(500)
004900000000     D*---
005000000000     D* DATI DI RIGHE IN STAMPA                          -MULTIPLE X 4 SPOOL-
005100100107     D BDAT            DS                  OCCURS(1) INZ
005200020806     D  LNP                    1     21  0 DIM(7)
005300020806     D  NRS                   22     35  0 DIM(7)
005400020806     D  NSP                   36     84  0 DIM(7)
005500020806     D  MGS                   85    112  0 DIM(7)
005600161212     D  VOL                  113    147  3 DIM(7)
005700161128     D  Spk                  148    196  1 DIM(7)
005800161128     D  Snc                  197    231  0 DIM(7)
005900161221     D  SPE                  232    238  0 DIM(7)
006000000000     D*---
006100161214     d  ri             s              3  0 INZ(0)
006200161214     d  savri          s              3  0 INZ(0)
006300161214     D  rieCOD         s              3  0 DIM(100) INZ(0)
006400161214     D  rieDES         s             40A   DIM(100) INZ(' ')
006500161214     d  rieVOL         s             13  3 DIM(100) INZ(0)
006600161214     d  riePES         s             13  1 DIM(100) INZ(0)
006700161214     d  rieNUM         s              9  0 DIM(100) INZ(0)
006800161221     d  rieSPE         s              7  0 DIM(100) INZ(0)
006900161214     D*---
007000000000     D* DS NUMERATORE ALLEGATI FATTURAZIONE INFRAGRUPPO
007100000000     D DS$5          E DS
007200000000     D*---
007300161221     D* DS TIPI BOLLE
007400161221     D dsTB          E DS
007500161221     d  Tipi_Bolla     s            100
007600161221     D ds2W          E DS
007700161221     d  FSP2WS         s              1    DIM(50)
007800161221     D*---
007900000000     D* DS CONTROLLA DATA (8)
008000000000     D WLBDA8          DS
008100000000     D  G08DAT                 1      8  0
008200000000     D  G08INV                 9     16  0
008300000000     D  G08ERR                17     17
008400000000     D  G08TGI                18     22  0
008500000000     D*---
008600000000     D* DS PARAMETRI LANCIO
008700170117     D PARAM         E DS                  extname(trec28ds)
008800170215     D*
008900170215     D*--- usati nell'SQL
009000161128     D titas         E DS                  extname(titas10f)
009100161128     D dar5fat       E DS
009200170215     D og143         E DS
009300170215     D*--- usati nell'SQL
009400170215     D*
009500161128     D KPJBA         E DS
009600161128     d campo           s             10
009700161128     d bolle           s             10
009800161201     d bollec          s             12
009900161128     d ar5             s             10
010000170104     d LINsql          s              3  0
010100161212     d VOLsql          s              5  3
010200161212     d PESsql          s              7  1
010300161206     d almenoUno       s              1  0
010400161213     d primavolta      s              1    INZ('S')
010500161128     d tipo            s            100
010600161219     d societa         s              3
010700170214     d savprtSOC       s                   like(prtsoc)
010800161219     d inputSOC        s                   like(prtsoc)
010900161219     d outputDES       s                   like(prtsocd)
011000170110     d DEPCode         s                   like(SAVCode)
011100161216     d Depsoc          s                   like(prtsoc)
011200161216     d Dessoc          s                   like(prtsocd)
011300161221     d contaD          s                   like(§$5nua)
011400161221     d contaR          s                   like(§$5nua)
011500161221     d NewNua          s                   like(§$5nua)
011600161207     ***************************************************************************
011700161207     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
011800161207     D ESITO_OK...
011900161207     D                 C                   0
012000161207     ***************************************************************************
012100161207     **
012200161207     ** Dichiarazione prototipi procedure esterne.
012300161207     **
012400161207     ***************************************************************************
012500161207      /DEFINE DFTACTGRP_YES
012600161207     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
012700161207      /UNDEFINE DFTACTGRP_YES
012800161207     ***************************************************************************
012900161207     **
013000161207     ** Definizione strutture dati.
013100161207     **
013200161207      **************************************************************************
013300161207     D tibsSocI0...
013400161207     D               E DS                  QUALIFIED
013500161207     D                                     INZ
013600161207     D tibsSocO0...
013700161207     D               E DS                  QUALIFIED
013800161207     D                                     INZ
013900161207     ***************************************************************************
014000161207     **
014100161207     ** Definizione variabili modulo/programma.
014200161207     **
014300161207     ***************************************************************************
014400161207     D prmRqsOpCode...
014500161207     D                 S             10A
014600161207     D prmRpyOpCode...
014700161207     D                 S             10A
014800161207     D prmRpyIdMsg...
014900161207     D                 S             10I 0
015000161207     D prmRqsFormato...
015100161207     D                 S             10A
015200161207     D prmRqsDataSize...
015300161207     D                 S             10I 0
015400161207     D prmRpyFormato...
015500161207     D                 S             10A
015600161207     D prmRpyDataSize...
015700161207     D                 S             10I 0
015800161207
015900000000     C*------------------------------------------------------------------------*
016000000000     C* MAIN LINE
016100000000     C*------------------------------------------------------------------------*
016200161214     C                   SETON                                        LR
016300161207     ** Inizializzo il programma.
016400161207     C                   CALL      'TIBSSOCR'
016500161207     C                   PARM      'INIT'        prmRqsOpCode
016600161207     C                   PARM                    prmRpyOpCode
016700161207     C                   PARM                    prmRpyIdMsg
016800161207     C*
016900170215     c                   clear                   almenoUno
017000170215     C*******
017100170215      *******  inizializza primo ciclo x SQL
017200170215     c*******            eval      bollec = 'TITAS10F.*'
017300170215     c*******            eval      bolle  = 'TITAS10F'
017400170215     c*******            eval         ar5 = 'FIAR500F'
017500170215      *******
017600170215      *******  Esegue 2 volte
017700170215     c*******             do        2
017800170215      **
017900170215      **   nell'istruzione legge sia TITAS10F che TITASP0F in UNION
018000161206     C                   EXSR      ISTRUZ
018100161214      **
018200161214     C                   Z-ADD     *ZEROS        TOTvolGEN        13 3
018300161214     C                   Z-ADD     *ZEROS        TOTpesGEN        13 1
018400161214     C                   Z-ADD     *ZEROS        TOTnclGEN         9 0
018500161221     C                   Z-ADD     *ZEROS        TOTspeGEN         7 0
018600161125     C/EXEC SQL
018700161125     C+ PREPARE S1 FROM :WrkSqlCmd
018800161125     C/END-EXEC
018900161125
019000161125     C/EXEC SQL
019100161125     C+ DECLARE A1 CURSOR FOR S1
019200161125     C/END-EXEC
019300161125
019400161125     C/EXEC SQL
019500161125     C+ OPEN A1
019600161125     C/END-EXEC
019700161214
019800161125     C/EXEC SQL
019900170104     C+ FETCH NEXT FROM A1 INTO :societa, :titas, :VOLsql, :PESsql, :LINsql
020000161125     C/END-EXEC
020100161214      **                    x errori
020200161214      * Forzo la stampa del JOBLOG.
020300170213     C                   If        SqlCod < 0
020400161214     C                   CALL      'X66CHGJOB'
020500161214     C                   RETURN
020600161214     C                   End
020700161214     C*  CICLO EOF                                           0°LIVELLO
020800161214     C                   DOW       SqlCod = 0
020900161214     c                   eval            depsoc = societa
021000161214     C                   EXSR      NEWSOC
021100161214      ***
021200161214     C* CICLO ROTTURA SOCIETA                                1°LIVELLO
021300161214     C                   DOW       SqlCod = 0 and DEPSoc = Societa
021400161214      ***  Imposta campo di confronto x 2°livello
021500161214     C                   exsr      Chiave2L
021600161214     c                   eval            savCode = DepCode
021700161214      ***
021800161214     C                   EXSR      NEWCODE
021900161214      ***
022000161214     C* CICLO ROTTURA TERM.PARTENZA / LNP / LNA              2°LIVELLO
022100161214     C                   DOW       SqlCod = 0 and DEPSoc = Societa
022200161214     c                              and  savCode = DepCode
022300170215     c                   eval      almenoUno = 1
022400170215      ***
022500161207     C                   EXSR      DeTTAGLIo
022600161212      ***
022700161214     C/EXEC SQL
022800170104     C+ FETCH NEXT FROM A1 INTO :societa, :titas, :VOLsql, :PESsql, :LINsql
022900161214     C/END-EXEC
023000161214     C                   exsr      Chiave2L                                 2L
023100161125     C                   ENDDO
023200161214      **  totali x CODE                                      2°LIVELLO
023300161214     C                   EXSR      TOT_COD
023400161214     C                   ENDDO
023500161214      **                                                     0°LIVELLO
023600161214      **  totali x SOCIETA                                   1°LIVELLO
023700161214     C                   EXSR      TOT_SOC
023800161214     C                   ENDDO
023900161125     C/EXEC SQL
024000161125     C+ CLOSE A1
024100161125     C/END-EXEC
024200161214      ** non c'era nulla  su TITAS10  deve provare su TITASP0
024300170215     c********           if        almenoUno = 0
024400170215     c********           eval      bollec = 'TITASP0F.*'
024500170215     c********           eval      bolle  = 'TITASP0F'
024600170215     c********           eval         ar5 = 'FIAR5P0F'
024700170215      ******** se Almeno uno c'è stato deve uscire
024800170215     C********           ElseIf    almenoUno = 1
024900170215     C********           LEAVE
025000170215     C********           End
025100170215      ********
025200170215E   1C********           ENDDO
025300170215      ********
025400161214      *    EoF
025500161214      **  totali x FINE FILE o GENERALI
025600161216     ** non si vogliono più i totali generali
025700161220     c                   if        Stampa_TGEN ='S'
025800161214     C                   EXSR      TOT_GEN
025900161214     C                   End
026000161214     c* ---------------------
026100161207     ** Chiudo il programma.
026200161207     C                   CALL      'TIBSSOCR'
026300161207     C                   PARM      'FINALIZE'    prmRqsOpCode
026400161207     C                   PARM                    prmRpyOpCode
026500161207     C                   PARM                    prmRpyIdMsg
026600161207     C*
026700161214     C                   Return
026800000000     C*------------------------------------------------------------------------*
026900161214     C* Chiave 2° LIVELLO   o TERMINAL PARTENZA o LNP o LNA
027000000000     C*------------------------------------------------------------------------*
027100161214     C     Chiave2L      BEGSR
027200161214      **
027300161214     C* Imposta la chiave specifica
027400170215      **  xchè chiamata in 3 modi diversi.
027500170215      **  nel campo DEPCODE  quindi  o TERMINAL o LINEA PART o ARR
027600170215     c*****************  select
027700170215     c*****************  when        partip = 'T'
027800170215     c*****************  eval      depcode=tastfp
027900170215     c*****************  when        partip = 'P'
028000170215     c*****************  eval      depcode=taslnp
028100170215     c*****************  when        partip = 'A'
028200170215     c*****************  eval      depcode= taslna
028300170215     c*****************  endsl
028400170215      *****************
028500170215      **
028600170116      ** Al momento si vuole raggruppato tutto sul TERMINAL
028700170116      **  (quindi serve per l'estero) come in ORDER BY aggiunto LINEA
028800170116      ** così adesso anche per eseguire le dovute rotture
028900170116     c                   eval      depcode= LINsql
029000161214      **
029100161214     C                   ENDSR
029200161214     C*------------------------------------------------------------------------*
029300161214     C* NEWCODE- OPERAZIONI PER NUOVO TERMINAL PARTENZA / LNP / LNA
029400161214     C*------------------------------------------------------------------------*
029500161214     C     NEWCODE       BEGSR
029600161214      **
029700161206     C                   Z-ADD     1             I
029800161207     C                   MOVEL     *BLANKS       DEScode
029900161207     C     DEPcode       LOOKUP    ORG(I)                                 28
030000161207     C   28              MOVEL     ODES(I)       DEScode
030100161214      **
030200161214     C* AZZERA LE VARIABILI DI LAVORO
030300161214     C                   Z-ADD     *ZEROS        TOTvolCOD        13 3
030400161214     C                   Z-ADD     *ZEROS        TOTpesCOD        13 1
030500161214     C                   Z-ADD     *ZEROS        TOTnclCOD         9 0
030600161221     C                   Z-ADD     *ZEROS        TOTspeCOD         7 0
030700990409     C*
030800000000     C                   ENDSR
030900161214     C*------------------------------------------------------------------------*
031000161214     C* NEWSOC - OPERAZIONI PER NUOVA SOCIETA'
031100161214     C*------------------------------------------------------------------------*
031200161214     C     NEWSOC        BEGSR
031300170125      **
031400161214     C* DECODIFICA la Società
031500161219     c                   eval      inputSOC = depSOC
031600161214     c                   exsr      Decod_societa
031700161219     c                   eval        desSOC = outputDES
031800161219      ***
031900161219     c                   if        partip = 'T'
032000161221     c                   eval        prtcli = inputSOC
032100161221     c                   eval        prtcliD= outputDES
032200161219     c                   else
032300161219     c                   eval        prtsoc = inputSOC
032400161219     c                   eval        prtsocD= outputDES
032500161219     c                   end
032600161219      ***
032700161219      ***  decodifica società passata fra i parametri
032800161219     c                   eval      inputSOC = parSOC
032900161219     c                   exsr      Decod_societa
033000161219     c                   if        partip = 'T'
033100161219     c                   eval        prtsoc = inputSOC
033200161219     c                   eval        prtsocD= outputDES
033300161219     c                   else
033400161221     c                   eval        prtcli = inputSOC
033500161221     c                   eval        prtcliD= outputDES
033600161219     c                   end
033700170214      **
033800170214     c*** a ogni cambio società - ri inizia a numerare le pagine
033900170214     c                   if        prtsoc <> savprtsoc
034000170214     c                   eval       savprtsoc = prtsoc
034100170214     c                   z-add     0             tecPAG
034200170214     c                   end
034300161214     C*
034400161214     C* AZZERA LE VARIABILI DI LAVORO
034500161214     C                   Z-ADD     *ZEROS        TOTVOLsoc        13 3
034600161214     C                   Z-ADD     *ZEROS        TOTpessoc        13 1
034700161214     C                   Z-ADD     *ZEROS        TOTnclsoc         9 0
034800161221     C                   Z-ADD     *ZEROS        TOTspesoc         7 0
034900161214     C                   Z-ADD     *ZEROS        RI
035000161214     C                   Z-ADD     *ZEROS        savRI
035100161214     c                   clear                   rieCOD
035200161214     c                   clear                   rieDES
035300161214     c                   clear                   rieVOL
035400161214     c                   clear                   riePES
035500161214     c                   clear                   rieNUM
035600161221     c                   clear                   rieSPE
035700161214     C*
035800161214     C                   ENDSR
035900161212     C*------------------------------------------------------------------------*
036000161214     C* TOT_COD - OPERAZIONI PER TOTALE CODICE  TERMINAL O LINEA
036100161212     C*------------------------------------------------------------------------*
036200161214     C     TOT_COD       BEGSR
036300161214     C*---
036400161214     C*  ULTIMO dettaglio
036500161214     C*   x esaurire schiere DETTAGLI
036600161214     C                   EXSR      STAMPAriga
036700161214      *--------------
036800161214     C*  DETTAGLIO
036900161214IF  2C     TOTvolCOD     IFNE      *ZEROS
037000161214     C                   Z-ADD     TOTvolCOD     TOCvol
037100161214     C                   Z-ADD     TOTpesCOD     TOCpes
037200161214     C                   Z-ADD     TOTnclCOD     TOCncl
037300161221     C                   Z-ADD     TOTspeCOD     TOCspe
037400161214      *
037500161214     c                   if        partip ='T'
037600161220     C                   MOVEL     STM(11)       TODDES
037700161214     c                   elseif    partip ='P'
037800161220     C                   MOVEL     STM(12)       TODDES
037900161214     c                   elseif    partip ='A'
038000161220     C                   MOVEL     STM(13)       TODDES
038100161214     c                   end
038200161214     c                   eval      todDES = %Trim(todDES) + ' ' +
038300161214     c                             %editc(savCODE:'X') + ' ' + DEScode
038400161214     C*
038500161214     C                   EXSR      testaDETT
038600161219     c                   if        stampa_dett ='S'
038700170117     c                             AND parSTP  ='S'
038800170110     C                   WRITE     DEEC28SE
038900161214     C                   WRITE     DEEC28TO
039000170110     C*******            WRITE     DEEC28SE
039100161219     c                   end
039200161214      *--------------
039300161214     C*  RIEPILOGO
039400161214     C                   Z-ADD     TOTvolCOD     DECvol
039500161214     C                   Z-ADD     TOTpesCOD     DECPes
039600161214     C                   Z-ADD     TOTnclCOD     DECncl
039700161221     C                   Z-ADD     TOTspeCOD     DECspe
039800161214     C                   MOVEL     SAVcode       DECcode
039900161214     C                   MOVEL     DEScode       DEDcode
040000161214      *
040100161214      *--- salva su schiera per riepilogare in dettaglio a TOT Società
040200161214     c                   add       1             ri
040300161214     c                   z-add     decCODE       rieCOD(RI)
040400161214     c                   movel     dedCODE       rieDES(RI)
040500161214     c                   z-add     TOTvolCOD     rieVOL(RI)
040600161214     c                   z-add     TOTpesCOD     riePES(RI)
040700161214     c                   z-add     TOTnclCOD     rieNUM(RI)
040800161221     c                   z-add     TOTspeCOD     rieSPE(RI)
040900161214     C                   Z-ADD     RI            savRI
041000161214      *---
041100161214     C                   EXSR      testaRIEP
041200161219     c                   if        stampa_riep ='S'
041300170117     c                             AND parSTP  ='S'
041400161214     C                   WRITE     RIEC27DE
041500161219     c                   end
041600161214E   2C                   ENDIF
041700161214     C*
041800161214     c                   seton                                        01
041900161214     C*
042000161214     C                   ENDSR
042100161207     C*------------------------------------------------------------------------*
042200161214     C*  - TOTALI per SOCIETA
042300161207     C*------------------------------------------------------------------------*
042400161214     C     TOT_SOC       BEGSR
042500161215     C*
042600161215IF  2C     TOTvolSOC     IFNE      *ZEROS
042700161220     C                   move      'TOTALE '     TODtot
042800161215     C                   Z-ADD     TOTvolSOC     TOCvol
042900161215     C                   Z-ADD     TOTpesSOC     TOCpes
043000161215     C                   Z-ADD     TOTnclSOC     TOCncl
043100161221     C                   Z-ADD     TOTspeSOC     TOCspe
043200161215     C* Riepilogo
043300161215     C                   EXSR      testaRIEP
043400161219     c                   if        stampa_riep ='S'
043500170117     c                             AND parSTP  ='S'
043600161215     C                   WRITE     RIEC27TO
043700161219     c                   end
043800161215     C* Dettaglio ------------
043900161219     c                   if        stampa_dett ='S'
044000170117     c                             AND parSTP  ='S'
044100161220     c                   seton                                        55
044200170116     c                   add       1             tecPAG
044300161215     C                   WRITE     DEEC28TE
044400161219     C                   WRITE     DEEC28SO
044500161215     C                   WRITE     DEEC28I3
044600161220     c                   setoff                                       55
044700161219     c                   end
044800161215      * ciclo riepilogo totali CODE
044900161215     c                   do        savri         ri
045000161215     c                   z-add     rieCOD(RI)    DE3CODE
045100161215     c                   movel     rieDES(RI)    DE3CODEd
045200161215     c                   z-add     rieVOL(RI)    DE3VOL
045300161215     c                   z-add     riePES(RI)    DE3PES
045400161215     c                   z-add     rieNUM(RI)    DE3NCL
045500161221     c                   z-add     rieSPE(RI)    DE3SPE
045600161219     c                   exsr      WRI_TNFIG
045700161219     c                   if        stampa_dett ='S'
045800170117     c                             AND parSTP  ='S'
045900161215     C                   WRITE     DEEC28D3
046000161219     c                   end
046100161215     c                   enddo
046200161215      **  totale società
046300161220     c                   eval      todDES = *blank
046400161219     c                   if        stampa_dett ='S'
046500170117     c                             AND parSTP  ='S'
046600170110     c                   seton                                        66
046700170110     C                   WRITE     DEEC28SE
046800161215     C                   WRITE     DEEC28TO
046900170110     c                   setoff                                       66
047000161219     c                   end
047100161215E   2C                   ENDIF
047200161215     C*
047300161215     c                   seton                                        0111
047400161214     C*
047500161214     C                   ENDSR
047600161214     C*------------------------------------------------------------------------*
047700161214     C*  - TOTALE GENERALE
047800161214     C*------------------------------------------------------------------------*
047900161214     C     TOT_GEN       BEGSR
048000161214     C*
048100161214     C* STAMPA RIMANENZE BOLLE E TOTALI TFP/TFA -DETTAGLIO E RIEPILOGO-
048200161214     c                   if        almenoUno = 1
048300161214      **
048400161214     C                   Z-ADD     1             S
048500161214     C     S             OCCUR     BDAT
048600161215     C******             EXSR      TOT_COD
048700161215     c******             exsr      TOT_SOC
048800161214     C*
048900161214     C* STAMPA IL TOTALE GENERALE E IL FINE ALLEGATO
049000161214     C                   EXSR      STotGEN
049100161214     C                   EXSR      STAFINE
049200161214     C*
049300161214     C* STAMPA LA SINTESI DELL'ELABORAZIONE
049400161219     c                   open      qsysprt
049500161214     c                   except    tes
049600161214     c                   except    det
049700161219     c                   close     qsysprt
049800161214     c                   end
049900161214     C*
050000161214     C                   ENDSR
050100161214     C*------------------------------------------------------------------------*
050200161214     C*  - MEMORIZZA E STAMPA DATI BOLLE
050300161214     C*------------------------------------------------------------------------*
050400161214     C     DEttagliO     BEGSR
050500000000     C*---
050600000000     C* CONTROLLA QUALE SPOOL CONSIDERARE
050700100107     C                   Z-ADD     1             S                 1 0
050800100107     C                   ADD       1             B1                1 0
050900100108     C                   Z-ADD     B1            B                 1 0
051000990419     C*
051100000000     C* MEMORIZZA I DATI DELLA BOLLA
051200990419     C     S             OCCUR     BDAT
051300161128     C                   Z-ADD     tasLNP        LNP(B)
051400161128     C                   Z-ADD     tasNRS        NRS(B)
051500161221     C                   Z-ADD     tasNSP        NSP(B)
051600161128     C                   Z-ADD     tasMGS        MGS(B)
051700161207     C                   Z-ADD     tasncl        Snc(B)
051800161221     C                   Z-ADD     1             SPE(B)
051900161212     C                   Z-ADD     VOLsql        VOL(B)
052000161206      *
052100161128     c* il volume è già risolto nell'istruzione sql mentre il peso no
052200161128     c* perchè ha più casistiche
052300161212     c                   if        PESsql <> 0
052400161212     C                   Z-ADD     PESsql        Spk(B)
052500161207     c                   else
052600161128     c                   if        tasfpf = 'V'
052700161128     C                   Z-ADD     taspkc        Spk(B)
052800161128     c                   else
052900161128     C                   Z-ADD     taspkf        Spk(B)
053000161128     c                   end
053100161128     c                   end
053200000000     C*
053300000000     C* CONTROLLA SE STAMPARE IL BLOCCO DELLE BOLLE MEMORIZZATE
053400000000IF  1C     S             IFEQ      1
053500161214     C                   ADD       VOLsql        TOTvolCOD
053600161212     C                   ADD       VOLsql        TOTVOLsoc
053700161214     C                   ADD       VOLsql        TOTVOLgen
053800161206      *
053900161214     C                   add       tasncl        totnclCOD
054000161207     C                   add       tasncl        totnclsoc
054100161214     C                   add       tasncl        totnclgen
054200161207      *
054300161221     C                   add       1             totspeCOD
054400161221     C                   add       1             totspesoc
054500161221     C                   add       1             totspegen
054600161221      *
054700161212     c                   if        PESsql <> 0
054800161212     C                   add       PESsql        totpessoc
054900161214     C                   add       PESsql        totpesCOD
055000161214     C                   add       PESsql        totpesGEN
055100161214      *
055200161207     c                   else
055300161207      *
055400161128     c                   if        tasfpf = 'V'
055500161214     C                   add       taspkc        totpesCOD
055600161214     C                   add       taspkc        totpesSOC
055700161214     C                   add       taspkc        totpesGEN
055800161128     c                   else
055900161214     C                   add       taspkf        totpesCOD
056000161214     C                   add       taspkf        totpesSOC
056100161214     C                   add       taspkf        totpesGEN
056200161128     c                   end
056300161128     c                   end
056400161206      *
056500161207     c* per traini sono 7 bolle su una riga , mentre per distribuz. 4
056600161128     c                   if        (partip = 'T' and B1 = 7) or
056700161212     c                             (partip <>'T' and B1 = 4)
056800161214     C                   EXSR      STAMPAriga
056900000000E   2C                   ENDIF
057000161206      *
057100000000E   1C                   ENDIF
057200161206      *
057300100107     C                   ENDSR
057400100107     C*------------------------------------------------------------------------*
057500161219     C* WRI_TNFIG- scrive wf per memorizzare i mc per terminal partenza
057600100107     C*------------------------------------------------------------------------*
057700161219     C     WRI_TNFIG     BEGSR
057800161207     C*
057900161221     c                   movel     prtcli        FIgcli
058000161221     c                   movel     prtclid       FIgcliD
058100161219     c                   movel     prtSOC        FIgSOC
058200161219     c                   movel     prtSOCd       FIgSOCD
058300161216     C                   Z-ADD     PARANN        FIgANNO
058400161216     C                   Z-ADD     PARMES        FIgMESE
058500161216     c                   movel     PARSIM        FIgSIM
058600161216     C                   movel     parTIP        FIgTIP
058700161216     C                   movel     DE3CODE       FIgTFP
058800161216     C                   movel     DE3CODEd      FIgTFPD
058900161216     C                   Z-ADD     DE3VOL        FIgVMC
059000161216     C                   Z-ADD     DE3PES        FIgPKG
059100161216     C                   Z-ADD     DE3NCL        FIgNCL
059200161221     C                   Z-ADD     DE3SPE        FIgSPE
059300161219     C                   WRITE     TNFIG000
059400100107     C*
059500000000     C                   ENDSR
059600000000     C*------------------------------------------------------------------------*
059700161214     C* STAMPAriga  - STAMPA IL BLOCCO DELLE BOLLE MEMORIZZATE
059800000000     C*------------------------------------------------------------------------*
059900161214     C     STAMPAriga    BEGSR
060000000000     C*
060100990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
060200000000IF  1C     LNP(1)        IFGT      *ZEROS
060300000000     C                   Z-ADD     LNP(1)        DECLN1
060400000000     C                   Z-ADD     NRS(1)        DECNR1
060500000000     C                   Z-ADD     NSP(1)        DECNS1
060600000000     C                   Z-ADD     MGS(1)        DECMG1
060700161212     C                   Z-ADD     VOL(1)        DECSM1
060800161207     C                   Z-ADD     SNC(1)        DECSC1
060900161207     C                   Z-ADD     SPK(1)        DECSP1
061000161207      *
061100000000     C                   Z-ADD     LNP(2)        DECLN2
061200000000     C                   Z-ADD     NRS(2)        DECNR2
061300000000     C                   Z-ADD     NSP(2)        DECNS2
061400000000     C                   Z-ADD     MGS(2)        DECMG2
061500161212     C                   Z-ADD     VOL(2)        DECSM2
061600161207     C                   Z-ADD     SNC(2)        DECSC2
061700161207     C                   Z-ADD     SPK(2)        DECSP2
061800161207      *
061900000000     C                   Z-ADD     LNP(3)        DECLN3
062000000000     C                   Z-ADD     NRS(3)        DECNR3
062100000000     C                   Z-ADD     NSP(3)        DECNS3
062200000000     C                   Z-ADD     MGS(3)        DECMG3
062300161212     C                   Z-ADD     VOL(3)        DECSM3
062400161207     C                   Z-ADD     SNC(3)        DECSC3
062500161207     C                   Z-ADD     SPK(3)        DECSP3
062600161207      *
062700000000     C                   Z-ADD     LNP(4)        DECLN4
062800000000     C                   Z-ADD     NRS(4)        DECNR4
062900000000     C                   Z-ADD     NSP(4)        DECNS4
063000000000     C                   Z-ADD     MGS(4)        DECMG4
063100161212     C                   Z-ADD     VOL(4)        DECSM4
063200161207     C                   Z-ADD     SNC(4)        DECSC4
063300161207     C                   Z-ADD     SPK(4)        DECSP4
063400161207      *
063500000000     C                   Z-ADD     LNP(5)        DECLN5
063600000000     C                   Z-ADD     NRS(5)        DECNR5
063700000000     C                   Z-ADD     NSP(5)        DECNS5
063800000000     C                   Z-ADD     MGS(5)        DECMG5
063900161212     C                   Z-ADD     VOL(5)        DECSM5
064000161207      *
064100000000     C                   Z-ADD     LNP(6)        DECLN6
064200000000     C                   Z-ADD     NRS(6)        DECNR6
064300000000     C                   Z-ADD     NSP(6)        DECNS6
064400000000     C                   Z-ADD     MGS(6)        DECMG6
064500161212     C                   Z-ADD     VOL(6)        DECSM6
064600161207      *
064700000000     C                   Z-ADD     LNP(7)        DECLN7
064800000000     C                   Z-ADD     NRS(7)        DECNR7
064900000000     C                   Z-ADD     NSP(7)        DECNS7
065000000000     C                   Z-ADD     MGS(7)        DECMG7
065100161212     C                   Z-ADD     VOL(7)        DECSM7
065200000000     C* FATTURE BAR
065300000000IF  2C     S             IFEQ      1
065400161213     C                   EXSR      testaDETT
065500161219     c                   if        stampa_dett ='S'
065600170117     c                             AND parSTP  ='S'
065700161214     C  n44              WRITE     DEEC28D1
065800161214     C   44              WRITE     DEEC28D2
065900161219     c                   end
066000000000     C                   Z-ADD     *ZEROS        B1
066100000000E   2C                   ENDIF
066200000000     C*
066300000000     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
066400161214     C                   CLEAR                   LNP
066500161214     C                   CLEAR                   NRS
066600161214     C                   CLEAR                   NSP
066700161214     C                   CLEAR                   MGS
066800161214     C                   CLEAR                   VOL
066900161214     C                   CLEAR                   Spk
067000161214     C                   CLEAR                   Snc
067100161221     C                   CLEAR                   SPE
067200161214     C*
067300000000E   1C                   ENDIF
067400000000     C*
067500000000     C                   ENDSR
067600990412     C*------------------------------------------------------------------------*
067700161213     C* STotGEN- STAMPA TOTALE GENERALE -DETTAGLIO-
067800000000     C*------------------------------------------------------------------------*
067900161213     C     STotGEN       BEGSR
068000000000     C*
068100161213     C* FATTURE BAR
068200161214IF  1C     TOTVOLgen     IFNE      *ZEROS
068300161213     C* Tot.Generale
068400161212     C                   MOVEL     STM(3)        TODDES
068500161213     C                   MOVEL     STM(3)        TODTOT
068600161220     C                   MOVE      'TOTALE'      TODTOT
068700161214     C                   Z-ADD     TOTvolGEN     TOCvol
068800161212     C*
068900161219     c                   if        stampa_dett ='S'
069000170117     c                             AND parSTP  ='S'
069100170116     c                   add       1             tecPAG
069200161214     C                   WRITE     DEEC28TE
069300170110     C                   WRITE     DEEC28SE
069400161214     C                   WRITE     DEEC28TO
069500161219     c                   end
069600161213     C*
069700161213     C*--- RIEPILOGO -------------------------
069800161213     c                   seton                                        33
069900161213     C                   EXSR      testaRIEP
070000161219     c                   if        stampa_riep ='S'
070100170117     c                             AND parSTP  ='S'
070200161214     C                   WRITE     RIEC27TO
070300161219     c                   end
070400161212E   1C                   ENDIF
070500000000     C*
070600000000     C                   ENDSR
070700000000     C*------------------------------------------------------------------------*
070800161213     C* testaDETT - STAMPA TESTATA DETTAGLIO
070900000000     C*------------------------------------------------------------------------*
071000161213     C     testaDETT     BEGSR
071100000000     C*
071200000000IF  1C     *IN01         IFEQ      *ON
071300161213      *---
071400161219     c                   if        stampa_dett ='S'
071500170117     c                             AND parSTP  ='S'
071600170116     c                   add       1             tecPAG
071700161214     C                   WRITE     DEEC28TE
071800161214     C                   WRITE     DEEC28SO
071900161214     C  n44              WRITE     DEEC28I1
072000161214     C   44              WRITE     DEEC28I2
072100161219     c                   end
072200000000     C                   MOVEL     '0'           *IN01
072300000000E   1C                   ENDIF
072400000000     C*
072500000000     C                   ENDSR
072600000000     C*------------------------------------------------------------------------*
072700161213     C* testaRIEP      - STAMPA TESTATA RIEPILOGO
072800000000     C*------------------------------------------------------------------------*
072900161213     C     testaRIEP     BEGSR
073000000000     C*
073100000000IF  1C     *IN11         IFEQ      *ON
073200161213      *
073300161219     c                   if        stampa_riep ='S'
073400170117     c                             AND parSTP  ='S'
073500161214     C                   WRITE     RIEC27TE
073600161214     C  N33              WRITE     RIEC27SO
073700161219     C                   WRITE     RIEC27IN
073800161219     c                   end
073900000000     C                   MOVEL     '0'           *IN11
074000000000E   1C                   ENDIF
074100000000     C*
074200000000     C                   ENDSR
074300000000     C*------------------------------------------------------------------------*
074400000000     C* *INZSR - OPERAZIONI INIZIALI
074500000000     C*------------------------------------------------------------------------*
074600000000     C     *INZSR        BEGSR
074700000000     C*---
074800000000     C* RICEVIMENTO PARAMETRI
074900000000     C     *ENTRY        PLIST
075000000000     C                   PARM                    KPJBA
075100000000     C                   MOVEL     KPJBU         PARAM
075200161221      **
075300161221     C     KEYTAB        KLIST
075400161221     C                   KFLD                    TBLKUT
075500161221     C                   KFLD                    TBLCOD
075600161221     C* Completa
075700161221     C     KTAB          KLIST
075800161221     C                   KFLD                    TBLKUT
075900161221     C                   KFLD                    TBLCOD
076000161221     C                   KFLD                    TBLKEY
076100161206      **
076200100108     C* REPERISCE LA DATA CORRENTE
076300100108     C                   TIME                    N14              14 0
076400100108     C                   MOVE      N14           DATA              8 0
076500100108     C                   MOVEL     N14           ORA               6 0
076600161213     C                   MOVEL     *all'_'       separa
076700161219      **-
076800161219     c                   if        stampa_dett = 'S'
076900161219     c                   open      TREC28DP
077000161219     c                   end
077100161219     c                   if        stampa_riep = 'S'
077200161219     c                   open      TREC28RP
077300161219     c                   end
077400161213      **-
077500161216     c                   if        partip ='T'
077600161216     C                   MOVEL     STM(3)        TECTES
077700161216     C                   MOVEL     STM(7)        campo25          25
077800161220     C                   MOVEL     STM(11)       IN3DES
077900161220     c                   evalr     tiposta = %Trim(STM(11))
078000161216     c                   elseif    partip ='P'
078100161216     C                   MOVEL     STM(4)        TECTES
078200161216     C                   MOVEL     STM(8)        campo25
078300161220     C                   MOVEL     STM(12)       IN3DES
078400161220     c                   evalr     tiposta = %Trim(STM(12))
078500161216     c                   elseif    partip ='A'
078600161216     C                   MOVEL     STM(4)        TECTES
078700161216     C                   MOVEL     STM(9)        campo25
078800161220     C                   MOVEL     STM(13)       IN3DES
078900161220     c                   evalr     tiposta = %Trim(STM(13))
079000161216     c                   end
079100161216      **-
079200161214     c* pulisce il periodo prima di aprire il file
079300161214     C/EXEC SQL
079400161219     C+ DELETE  FROM TNfig00f  WHERE  FigSIM = :PARSIM and
079500161216     C+   FIgANNO = :PARANN  and FIgMESE = :PARMES
079600161216     C+                      and FIgTIP  = :PARTIP
079700161214     C/END-EXEC
079800161219     c                   open      TNFIG00F
079900161216      **-
080000161213     c                   eval      tecTES = %trim(tecTES) +
080100161220     c                             '  ' + campo25
080200161220     c                   eval      tecTES = %trim(tecTES) +
080300161220     c                             '  PERIODO ' + %editc(parMES:'X') +
080400161220     c                             '/' + %editc(parANN:'X')
080500161220      ****
080600161214     C                   MOVEL     campo25       INDDES
080700000000     C*--------------------
080800000000     C* VARIABILI DI CONTROLLO
080900000000     C*--------------------
081000000000     C                   MOVEL     'N'           $FINE             1
081100000000     C                   MOVEL     'N'           WRECOK            1
081200000000     C                   MOVEL     '1'           *IN01
081300000000     C                   MOVEL     '1'           *IN11
081400161213     C                   MOVEL     '0'           *IN44
081500161213     C*
081600161212     c                   if        partip <>'T'
081700161213     C                   MOVEL     '1'           *IN44
081800161212     c                   end
081900000000     C*
082000000000     C* REPERISCE LA DATA CORRENTE
082100000000     C                   TIME                    N14              14 0
082200000000     C                   MOVE      N14           N8                8 0
082300000000     C                   Z-ADD     N8            G08DAT
082400000000     C                   Z-ADD     *ZEROS        G08INV
082500000000     C                   MOVEL     '0'           G08ERR
082600000000     C                   CALL      'XSRDA8'
082700000000     C                   PARM                    WLBDA8
082800000000     C                   Z-ADD     G08INV        DATCOR            8 0
082900161216     c*---
083000161216     c* ORGANIGRAMMA
083100161216     C                   Z-ADD     *ZEROS        I                 5 0
083200161216     C     *LOVAL        SETLL     AZORG01L
083300161216     C                   READ      AZORG01L                               99
083400161216DO  1C     *IN99         DOWEQ     *OFF
083500161216IF  2C     ORGFVA        IFEQ      *BLANKS
083600161216     C     ORGFAG        ANDNE     'V'
083700161216     C                   ADD       1             I
083800161216     C                   Z-ADD     ORGFIL        ORG(I)
083900161216     C                   MOVEL     ORGDES        ODES(I)
084000161216E   2C                   ENDIF
084100161216     C                   READ      AZORG01L                               99
084200161216E   1C                   ENDDO
084300000000     C*
084400000000     C* DECODIFICA I PARAMETRI DI LANCIO
084500161212     C                   MOVEL     'TREC28R'     TECpgm
084600161212IF  1C     PARSIM        IFEQ      'S'
084700161212     C                   MOVEL     STM(10)       TEDSIM
084800161212X   1C                   ELSE
084900161212     C                   MOVEL     *BLANKS       TEDSIM
085000161212E   1C                   ENDIF
085100161221     C*---
085200161221     C* TIPOLOGIA TIPI BOLLE
085300161221     c                   clear                   W2                3 0
085400161221     c                   clear                   FSP2WS
085500161221     C                   Z-ADD     1             TBLKUT
085600161221     C                   MOVEL     '2W'          TBLCOD
085700161221     C     KEYTAB        SETLL     TABEL00F
085800161221     C     KEYTAB        READE(n)  TABEL00F
085900161221     c                   dow       not %EoF(Tabel00F)
086000161221     c                   movel     tblUNI        ds2W
086100161221     c                   if        §2WFSP='S'
086200161221     c                   add       1             W2
086300161221     c                   movel     tblkey        FSP2WS(W2)
086400161221     c                   end
086500161221     C     KEYTAB        READE(n)  TABEL00F
086600161221     c                   enddo
086700161221     C*---
086800161221     C* TIPI BOLLE DA CONSIDERARE
086900161221     C                   Z-ADD     1             TBLKUT
087000161221     C                   MOVEL     'TB'          TBLCOD
087100161221     c                   clear                   Tipi_Bolla
087200161221     C     KEYTAB        SETLL     TABEL00F
087300161221     C     KEYTAB        READE(n)  TABEL00F
087400161221     c                   dow       not %EoF(Tabel00F)
087500161221     c                   movel     tblUNI        dsTB
087600161221      * sulla precedente tabella (a Spedizione)
087700161221     c     §TBRBL        lookup    FSP2WS                                 21
087800161221     c                   if        *in21
087900161221     c                   eval      Tipi_Bolla = %trim(Tipi_Bolla) +
088000161221     c                             ' ''' + %trim(tblkey) + ''', '
088100161221     c                   end
088200161221     C     KEYTAB        READE(n)  TABEL00F
088300161221     c                   enddo
088400161221     c                   clear                   Lung              3 0
088500161221      *
088600161221      *  Quanti caratteri ha inserito nel campo ?
088700161221     c                   eval      Lung = %Len(%Trim(Tipi_Bolla))
088800161221      * ultima virgola aggiunta
088900161221      *   deve trasformarla in blank
089000161221     c                   if        Lung >0
089100161221     c                   if        %subst(Tipi_Bolla:Lung:1)=','
089200161221     c                   eval      %subst(Tipi_Bolla:Lung:1)=*blank
089300161221     c                   end
089400161221     c                   end
089500161221     C*---
089600161221     C* NUMERATORE ALLEGATI FATTURAZIONE INFRAGRUPPO
089700161221     C                   Z-ADD     1             TBLKUT
089800161221     C                   MOVEL     '$5'          TBLCOD
089900161221     c                   movel     partip        campo5            5
090000161221     c                   move      parann        campo5
090100161221     c                   movel     campo5        campo7            7
090200161221     c                   move      parmes        campo7
090300161221     C                   MOVEL(P)  campo7        TBLKEY
090400161221     C                   CLEAR                   DS$5
090500161221     C     KTAB          CHAIN     TABEL00F
090600161221IF  1C                   IF        %Found(Tabel00F)
090700161221     C                   MOVEL     TBLUNI        DS$5
090800161221E   1C                   ENDIF
090900161221     C*---
091000161221     C* CALCOLA IL NUMERATORE PER TUTTI GLI ALLEGATI
091100161221     c                   if        stampa_dett = 'S'
091200161221     C     §$5NUA        ADD       1             contaD
091300161221     C                   Z-ADD     contaD        NEWNUA
091400161221     c                   end
091500161221      *
091600161221     c                   if        stampa_riep = 'S'
091700161221     C     contaD        ADD       1             contaR
091800161221     C                   Z-ADD     contaR        NEWNUA
091900161221     c                   end
092000161221      *
092100161221     C*
092200161221     C* AGGIORNA IL NUMERATORE
092300161221IF  1C     PARSIM        IFEQ      'N'
092400161221IF  1C                   IF        %Found(Tabel00F)
092500161221     C                   Z-ADD     NEWNUA        §$5NUA
092600161221     C                   MOVEL     DS$5          TBLUNI
092700161221     C                   UPDATE    TABEL
092800161221X   2C                   ELSE
092900161221     C                   Z-ADD     1             TBLKUT
093000161221     C                   MOVEL     '$5'          TBLCOD
093100161221     c                   movel     partip        campo5
093200161221     c                   move      parann        campo5
093300161221     c                   movel     campo5        campo7            7
093400161221     c                   move      parmes        campo7
093500161221     C                   MOVEL(p)  campo7        TBLKEY
093600161221     C                   Z-ADD     NEWNUA        §$5NUA
093700161221     C                   eval         TBLUNI = DS$5
093800161221     C                   WRITE     TABEL
093900161221E   2C                   ENDIF
094000161221X   1C                   ELSE
094100161221     C     KTAB          SETGT     TABEL00F
094200161221E   1C                   ENDIF
094300161221     C*
094400000000     C                   ENDSR
094500161206     C*--------------------------------------------------------------**
094600161213     C*   Decodifica Società
094700161213     C*--------------------------------------------------------------**
094800161207     C     Decod_societa begSR
094900161219     C                   clear                   outputDES
095000161207      *
095100161207     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
095200161207     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
095300161207     C                   RESET                   tibsSocI0
095400161219     C                   EVAL      tibsSocI0.idSocieta = inputSOC
095500161207     C
095600161207     C                   CALL      'TIBSSOCR'
095700161207     C                   PARM      'GETANAGRAF'  prmRqsOpCode
095800161207     C                   PARM      *BLANK        prmRpyOpCode
095900161207     C                   PARM      *ZERO         prmRpyIdMsg
096000161207     C                   PARM      'TIBSSOCI0'   prmRqsFormato
096100161207     C                   PARM                    tibsSocI0
096200161207     C                   PARM                    prmRqsDataSize
096300161207     C                   PARM      'TIBSSOCO0'   prmRpyFormato
096400161207     C                   PARM                    tibsSocO0
096500161207     C                   PARM                    prmRpyDataSize
096600161207     c                   if         PRMRPYIDMSG >= 0
096700161207     c                              and TIBSSOCO0.IDSOCIETA <> *blank
096800161219     c                   eval      outputDES = tibsSocO0.RAGSOCIALE
096900161207     c                   else
097000161219     c                   eval      outputDES = 'Errore'
097100161207     c                   end
097200161207      *
097300161207     C                   ENDSR
097400161212?     *--------------------------------------------------------------*
097500161212?     *  istruz: Operazioni di inizializzazione dati                 *
097600161212?     *--------------------------------------------------------------*
097700161212     C     istruz        BEGSR
097800161212     **
097900170215?     *-------------------------------------
098000170215     **      3 CASI : come viene sviluppato l'SQL sotto:
098100170215     **  x TERMINAL PARTENZA
098200170215?     *-------------------------------------
098300170215     **  WITH
098400170215     **  LINEEITA  as
098500170215     **  (SELECT orgfil FROM AZORG00F  WHERE
098600170215     **  substr(orgde3, 11, 3) in  (SELECT substr(tbeke1, 1, 3) FROM
098700170215     **  tntbe00f WHERE tbecod='NTW' and substr(tbeuni, 31, 1) <>'E')
098800170215     **    and  orgfag <> 'V'
098900170215     **   ),
099000170215     **  SPEDIZIONI as   (
099100170215     **  (SELECT TITAS10F.*  ,
099200170215     **  CASE WHEN dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
099300170215     **  is null THEN
099400170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
099500170215     **  END AS VOL,
099600170215     **  CASE WHEN dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1)
099700170215     **  is null THEN 0 ELSE
099800170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
099900170215     **  tasTFP as LINEA
100000170215     **  FROM  TITAS10F left outer JOIN FIAR500F
100100170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
100200170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
100300170215     **  left outer JOIN  LINEEITA  on tasTFP = orgfil
100400170215     **  WHERE
100500170215     **   tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02 and
100600170215     **  tastbl in ('AS', 'A2', 'FS', 'F1')    )
100700170215     **      UNION
100800170215     **  (SELECT TITASP0F.* ,
100900170215     **  CASE WHEN dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
101000170215     **   is null THEN tasvlf ELSE
101100170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) END AS VOL,
101200170215     **  CASE WHEN dec(dec(substr(ar5uni, 2,  7), 7)/10, 7, 1)
101300170215     **  is null THEN 0 ELSE
101400170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
101500170215     **  tasTFP as LINEA
101600170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
101700170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
101800170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
101900170215     **  left outer JOIN  LINEEITA  on tasTFP = orgfil
102000170215     **  WHERE
102100170215     **  tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02 and
102200170215     **  tastbl in ('AS', 'A2', 'FS', 'F1')     )
102300170215     **  )
102400170215     **
102500170215     **  SELECT ID_SOCIETA, SPEDIZIONI.* FROM
102600170215     **  SPEDIZIONI join AZSOF00F on LINEA = ID_FILIALE WHERE
102700170215     **  VALIDITA_DATA_INIZIO <=
102800170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
102900170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
103000170215     **  VALIDITA_DATA_FINE >=
103100170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
103200170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)
103300170215     **  and id_societa <> '219' and tastfp <> tastfa ORDER BY
103400170215     **  ID_SOCIETA , LINEA , tastfp, tasaas , tasmgs, tasLNP
103500170215     **
103600170215?     *--------------------------------------------------------------
103700170215     **  x LINEA ARRIVO
103800170215?     *-------------------------------------
103900170215     **  WITH
104000170215     **  LINEEITA  as (
104100170215     **  SELECT orgfil FROM AZORG00F WHERE  orgfag <> 'V'  and
104200170215     **  substr(orgde3, 11, 3) IN  (SELECT substr(tbeke1, 1, 3)
104300170215     **  FROM  tntbe00f WHERE tbecod='NTW' and
104400170215     **  substr(tbeuni, 31, 1) <>'E')
104500170215     **  ),
104600170215     **  SPEDIZIONI as (
104700170215     **  (SELECT TITAS10F.*, CASE WHEN
104800170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
104900170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
105000170215     **  END AS VOL, CASE WHEN
105100170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
105200170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
105300170215     **  CASE WHEN orgfil is null
105400170215     **  THEN tasTFA  ELSE tasLNA END as LINEA
105500170215     **  FROM  TITAS10F left outer JOIN FIAR500F
105600170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp
105700170215     **  and ar5nrs =  tasnrs and ar5nsp = tasnsp
105800170215     **  left outer JOIN  LINEEITA  on tasLNA = orgfil
105900170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
106000170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
106100170215     **  UNION
106200170215     **  (SELECT TITASP0F.*, CASE WHEN
106300170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
106400170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
106500170215     **  END AS VOL, CASE WHEN
106600170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
106700170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
106800170215     **  CASE WHEN  orgfil is null
106900170215     **  THEN tasTFA  ELSE tasLNA END as LINEA
107000170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
107100170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp
107200170215     **  and ar5nrs =  tasnrs and ar5nsp = tasnsp
107300170215     **  left outer JOIN  LINEEITA  on tasLNA = orgfil
107400170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
107500170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
107600170215     **  )
107700170215     **
107800170215     **  SELECT ID_SOCIETA, SPEDIZIONI.*
107900170215     **  FROM SPEDIZIONI join AZSOF00F
108000170215     **  on LINEA = ID_FILIALE
108100170215     **  WHERE  VALIDITA_DATA_INIZIO <=
108200170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
108300170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
108400170215     **  VALIDITA_DATA_FINE >=
108500170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
108600170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)   and
108700170215     **  id_societa <> '201'
108800170215     **  ORDER BY ID_SOCIETA , LINEA , taslna, tasaas , tasmgs
108900170215     **
109000170215?     *--------------------------------------------------------------
109100170215     **  x LINEA PARTENZA
109200170215?     *-------------------------------------
109300170215     **  WITH
109400170215     **  LINEEITA  as (
109500170215     **  SELECT orgfil FROM AZORG00F WHERE  orgfag <> 'V'  and
109600170215     **  substr(orgde3, 11, 3) IN  (SELECT substr(tbeke1, 1, 3)
109700170215     **  FROM  tntbe00f
109800170215     **  WHERE tbecod='NTW' and  substr(tbeuni, 31, 1) <>'E')
109900170215     **  ),
110000170215     **  SPEDIZIONI as (
110100170215     **  (SELECT TITAS10F.*, CASE WHEN
110200170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
110300170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
110400170215     **  END AS VOL, CASE WHEN
110500170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
110600170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
110700170215     **  CASE WHEN  orgfil is null
110800170215     **  THEN tasTFP  ELSE tasLNP END as LINEA
110900170215     **  FROM  TITAS10F left outer JOIN FIAR500F
111000170215     **  ON ar5trd='FAT' and ar5aas = tasaas and ar5lnp = taslnp and
111100170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
111200170215     **  left outer JOIN  LINEEITA  on tasLNP = orgfil
111300170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
111400170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
111500170215     **  UNION
111600170215     **  (SELECT TITASP0F.* , CASE WHEN
111700170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
111800170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
111900170215     **  END AS VOL, CASE WHEN
112000170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
112100170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
112200170215     **  CASE WHEN  orgfil is null
112300170215     **  THEN tasTFP  ELSE tasLNP END as LINEA
112400170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
112500170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
112600170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
112700170215     **  left outer JOIN  LINEEITA  on tasLNP = orgfil
112800170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
112900170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
113000170215     **   )
113100170215     **
113200170215     **  SELECT ID_SOCIETA, SPEDIZIONI.* FROM
113300170215     **  SPEDIZIONI join AZSOF00F on LINEA = ID_FILIALE
113400170215     **  WHERE  VALIDITA_DATA_INIZIO <=
113500170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
113600170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
113700170215     **  VALIDITA_DATA_FINE >=
113800170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
113900170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)   and
114000170215     **  id_societa <> '201'
114100170215     **  ORDER BY ID_SOCIETA , LINEA , taslnp, tasaas , tasmgs
114200170215     **
114300170215?     *--------------------------------------------------------------
114400170215     **  L'istruzione SQL si compone di una parte preparatoria dei DATI
114500170215     **   con la WITH.
114600170215     **    il primo file : LINEEITA   (solo le LINEE ITALIA no ESTERO)
114700170215     **    il secondo    : SPEDIZIONI (da TITAS10F e TITASP0F con i loro AR5)
114800170215     **
114900170215     **  Risolte nella WITH i dati essenziali all'elaborazione, l'istruzione
115000170215     **   SQL si semplifica estremamente nel ciclo del programma.
115100170215      **
115200161212     c                   clear                   tipo
115300161212     c                   clear                   campo
115400170215      ** ----------------------
115500170215      **  In Funzione del tipo di Lancio del programma:
115600170215      **   x Terminal / Linea Partenza / Linea Arrivo
115700170215      **    imposto le parti variabili necessarie all'SQL in funzione del
115800170215      **    tipo di chiamata del programma.
115900170215      **
116000170215      **   Dentro: "TIPO" e "CAMPO" le stringhe da concattare nell'SQL.
116100170215      **
116200161212     c                   select
116300170215      ** x Terminal
116400170215     c                   when      partip = 'T'
116500161213     c                   eval      tipo = ' and tastfp <> tastfa '
116600170109     c                   eval      campo = 'tastfp'
116700170215      ** x Linea di Partenza
116800161212     c                   when      partip = 'P'
116900161212     c                   eval      campo = 'taslnp'
117000170215      ** x Linea di Arrivo
117100161212     c                   when      partip = 'A'
117200161212     c                   eval      campo = 'taslna'
117300161212     c                   endsl
117400170109      ****
117500170109      *  nella WITH in LINEEITA si carica tutte le LINEE NON Estere
117600170109      *    dall'Organigramma (selezionate in base al Network)
117700170109      *             in SPEDIZIONI ha già selezionato TITAS ricavando la
117800170109      *    linea del Terminal come relativa filiale ITALIA in funzione del
117900170215      *    tipo di elaborazione .
118000170109      ****
118100170215     C                   EVAL      WrkSqlCmd ='WITH '                       +
118200170215      *-
118300170215      *  prende dall'Organigramma le linee con Network presente in tabella "NTW"
118400170215      *  che NON sono "ESTERO"
118500170215     C                             ' LINEEITA  as ('                        +
118600170215     C                               ' SELECT orgfil FROM AZORG00F WHERE '  +
118700170215     C                             ' orgfag <> ''V''  and '                 +
118800170215     C                             ' substr(orgde3, 11, 3) IN '             +
118900170215     C                             ' (SELECT substr(tbeke1, 1, 3) FROM '    +
119000170109     C                             ' tntbe00f WHERE tbecod=''NTW'' and '    +
119100170215     C                             ' substr(tbeuni, 31, 1) <>''E'') '       +
119200170215     C                             ' ),   '                                 +
119300170215      *-
119400170215      *   LEGGE TITAS ma utilizza il suo FIAR5 x risolvere i PESI e VOLUMI
119500170215      *    necessari al calcolo.
119600170215     C                             ' SPEDIZIONI as ('                       +
119700170215     C                               ' (SELECT TITAS10F.* , CASE WHEN '     +
119800170104     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
119900170104     C                             ' 5, 3) is null THEN tasvlf ELSE '       +
120000170104     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
120100170104     C                             ' 5, 3) END AS VOL, CASE WHEN '          +
120200170104     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
120300170104     C                             ' 1) is null THEN 0 ELSE '               +
120400170104     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
120500170109     C                             ' 1) END AS PES, '
120600170215      ****
120700170215      * In funzione del tipo di chiamata, imposta un campo "LINEA" generico
120800170109      *    x terminal di PARTENZA oppure per linea sostituisce se ESTERO
120900170109      *    con relativa filiale presente sul terminal.
121000170109      ****
121100170109     c                   if        partip ='T'
121200170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
121300170109     C                             ' tasTFP '
121400170109     c                   elseif    partip ='P'
121500170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
121600170109     C                             ' CASE WHEN  orgfil is null THEN tasTFP '+
121700170109     C                             ' ELSE tasLNP END '
121800170109     c                   elseif    partip ='A'
121900170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
122000170109     C                             ' CASE WHEN  orgfil is null THEN tasTFA '+
122100170109     C                             ' ELSE tasLNA END '
122200170109     c                   end
122300170109      ****
122400170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
122500170215     C                             ' as LINEA    FROM '                     +
122600170215      ****  LEFT OUTER JOIN
122700170215     C                             ' TITAS10F left outer JOIN FIAR500F '    +
122800170215     C                             ' ON ar5trd=''FAT'' and ar5aas = tasaas '+
122900170215     C                             ' and ar5lnp = taslnp and ar5nrs = '     +
123000170215     C                             ' tasnrs and ar5nsp = tasnsp '           +
123100170215      ****  LEFT OUTER JOIN
123200170215     C                             ' left outer JOIN  LINEEITA  on '
123300170215      *  in funzione del TIPO
123400170110     c                   if        partip ='T'
123500170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
123600170215     C                             ' tasTFP = orgfil '
123700170110     c                   elseif    partip ='P'
123800170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
123900170215     C                             ' tasLNP = orgfil '
124000170110     c                   elseif    partip ='A'
124100170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
124200170215     C                             ' tasLNA = orgfil '
124300170110     c                   end
124400170215      ****  Nella WHERE i range dei Parametri ricevuti
124500170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
124600170215     C                             ' WHERE tasaas = ' + %editc(parann:'X')  +
124700170104     C                             ' and substr(digits(tasmgs), 1, 2) = '   +
124800170104     C                             %editc(parmes:'X') + ' and tastbl in ('  +
124900170215     C                             %trim(Tipi_Bolla)  + ') ) '              +
125000170215      **
125100170215      ** si mette in UNION con il TITASP
125200170215     C                             '  UNION  '                              +
125300170215      **
125400170215      **  IDENTICO a quanto scritto sopra ma con i file versione (P)
125500170215     C                               ' (SELECT TITASP0F.* , CASE WHEN '     +
125600170215     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
125700170215     C                             ' 5, 3) is null THEN tasvlf ELSE '       +
125800170215     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
125900170215     C                             ' 5, 3) END AS VOL, CASE WHEN '          +
126000170215     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
126100170215     C                             ' 1) is null THEN 0 ELSE '               +
126200170215     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
126300170215     C                             ' 1) END AS PES, '
126400170215      ****
126500170215      * In funzione del tipo di chiamata, imposta un campo "LINEA" generico
126600170215      *    x terminal di PARTENZA oppure per linea sostituisce se ESTERO
126700170215      *    con relativa filiale presente sul terminal.
126800170215      ****
126900170215     c                   if        partip ='T'
127000170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
127100170215     C                             ' tasTFP '
127200170215     c                   elseif    partip ='P'
127300170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
127400170215     C                             ' CASE WHEN  orgfil is null THEN tasTFP '+
127500170215     C                             ' ELSE tasLNP END '
127600170215     c                   elseif    partip ='A'
127700170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
127800170215     C                             ' CASE WHEN  orgfil is null THEN tasTFA '+
127900170215     C                             ' ELSE tasLNA END '
128000170215     c                   end
128100170215      ****
128200170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
128300170215     C                             ' as LINEA    FROM '                     +
128400170215      ****  LEFT OUTER JOIN
128500170215     C                             ' TITASP0F left outer JOIN FIAR5P0F '    +
128600170215     C                             ' ON ar5trd=''FAT'' and ar5aas = tasaas '+
128700170215     C                             ' and ar5lnp = taslnp and ar5nrs = '     +
128800170215     C                             ' tasnrs and ar5nsp = tasnsp '           +
128900170215      ****  LEFT OUTER JOIN
129000170215     C                             ' left outer JOIN  LINEEITA  on '
129100170215      *  in funzione del TIPO
129200170215     c                   if        partip ='T'
129300170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
129400170215     C                             ' tasTFP = orgfil '
129500170215     c                   elseif    partip ='P'
129600170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
129700170215     C                             ' tasLNP = orgfil '
129800170215     c                   elseif    partip ='A'
129900170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
130000170215     C                             ' tasLNA = orgfil '
130100170215     c                   end
130200170215      ****  Nella WHERE i range dei Parametri ricevuti
130300170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
130400170215     C                             ' WHERE tasaas = ' + %editc(parann:'X')  +
130500170215     C                             ' and substr(digits(tasmgs), 1, 2) = '   +
130600170215     C                             %editc(parmes:'X') + ' and tastbl in ('  +
130700170215     C                             %trim(Tipi_Bolla)  + ') ) '              +
130800170215     C                             '  )  '                                  +
130900170215      **
131000170215      ** --------------
131100170215      **  DA QUI l'Istruzione di LETTURA da utilizzare nel ciclo di DETTAGLIO
131200170215      **  collegata alla SOCIETA in JOIN  (già semplificata da quanto definito
131300170215      **    nel WITH.
131400170215      ** --------------
131500170215     C                             'SELECT ID_SOCIETA, SPEDIZIONI.* FROM '  +
131600170104     C                             'SPEDIZIONI join AZSOF00F on LINEA '     +
131700170104     C                             '= ID_FILIALE WHERE '                    +
131800170104     C                             ' VALIDITA_DATA_INIZIO <= varchar(DATE(' +
131900170104     C                             'TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas), +
132000170104     C                              DIGITS(tasmgs)), ''YYYYMMDD'') ) , iso) +
132100170104     C                             and VALIDITA_DATA_FINE >= varchar(DATE(' +
132200170104     C                             'TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas), +
132300170104     C                             DIGITS(tasmgs)), ''YYYYMMDD'')), iso)    +
132400170104     C                             and id_societa <> ''' + parsoc + ''' '   +
132500170215      *
132600170215      *-  ORDINAMENTO in funzione del TIPO chiamata
132700170104     C                             %trim(tipo) + ' ORDER BY ID_SOCIETA , '  +
132800170116     C                             'LINEA , '                               +
132900170116     C                             %trim(campo) + ', tasaas , tasmgs'
133000170109      *-
133100170215     c                   if        partip ='T'
133200170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
133300170109     C                             ', tasLNP '
133400170109     C                   END
133500170109      *-
133600161212     C                   ENDSR
133700161212     C*------------------------------------------------------------------------*
133800161213     C* STAFINE- STAMPA FINE PROSPETTO
133900161212     C*------------------------------------------------------------------------*
134000161213     C     STAFINE       BEGSR
134100161219      *--
134200161219     c                   if        stampa_dett ='S'
134300170117     c                             AND parSTP  ='S'
134400161214     C                   WRITE     DEEC28FI
134500161219     c                   end
134600161219      *--
134700161219     c                   if        stampa_riep ='S'
134800170117     c                             AND parSTP  ='S'
134900161214     C                   WRITE     RIEC27FI
135000161219     c                   end
135100161219      *--
135200161212     C                   ENDSR
135300161213     C*------------------------------------------------------------------------*
135400100107     OQSYSPRT   E            TES            2 02
135500100107     O                       STM(1)              66
135600100107     O                       STM(2)             132
135700100107     O                       PARANN        Z     80
135800100107     O                       PARMES        Z     83
135900100107     O                       TEDSIM              99
136000100107     O                       DATA          8    111
136100100107     O                       ORA                121 '0 :  :  '
136200100107     O                       PAGE          Z    132
136300100107     O          E            DET         1
136400100107     O                       STM(5)              66
136500100127     O                       parann        Z     39
136600161221     O                       contaD              42
136700161214     O                       TOTVOLGEN     O     75
136800161212     o                                           80 '(MC)'
136900161214     O                  44   TOTPESGEN     O    100
137000161213     o                  44                      105 '(KG)'
137100161214     O                  44   TOTNCLGEN     O    125
137200161213     o                  44                      130 '(NR)'
137300100107     O          E            DET         1
137400100107     O                       STM(6)              66
137500100127     O                       parann        Z     39
137600161221     O                       contaR              42
137700161214     O                       TOTVOLGEN     O     75
137800161212     o                                           80 '(MC)'
137900161214     O                  44   TOTPESGEN     O    100
138000161213     o                  44                      105 '(KG)'
138100161214     O                  44   TOTNCLGEN     O    125
138200161213     o                  44                      130 '(NR)'
138300000000     O*------------------------------------------------------------------------*
138400990420**   STM - DECODIFICHE RIGHE DI STAMPA
138500161128TREC28R                       SINTESI FATTURAZIONE             MC 1
138600100107ANNO MESE 9999/99     XXXXXXXXXXX  99/99/9999  99:99:99   PAG.99992
138700161216ALLEGATO                                                          3
138800161216ALLEGATO DISTRIBUZIONE                                            4
138900161216DETTAGLIO                                                         5
139000161216RIEPILOGO                                                         6
139100161216TRAINI                                                            7
139200161216PARTENZA                                                          8
139300161216ARRIVO                                                            9
139400990420SIMULAZIONE                                                       10
139500161220TERMINAL PARTENZA                                                 11
139600170117LINEA/HUB PARTENZA                                                12
139700170117LINEA/HUB ARRIVO                                                  13
139800161220                                                                  14
139900161220                                                                  15
