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
017000170215      **   nell'istruzione legge sia TITAS10F che TITASP0F in UNION
017100161206     C                   EXSR      ISTRUZ
017200161214     C                   Z-ADD     *ZEROS        TOTvolGEN        13 3
017300161214     C                   Z-ADD     *ZEROS        TOTpesGEN        13 1
017400161214     C                   Z-ADD     *ZEROS        TOTnclGEN         9 0
017500161221     C                   Z-ADD     *ZEROS        TOTspeGEN         7 0
017600161125     C/EXEC SQL
017700161125     C+ PREPARE S1 FROM :WrkSqlCmd
017800161125     C/END-EXEC
017900161125
018000161125     C/EXEC SQL
018100161125     C+ DECLARE A1 CURSOR FOR S1
018200161125     C/END-EXEC
018300161125
018400161125     C/EXEC SQL
018500161125     C+ OPEN A1
018600161125     C/END-EXEC
018700161214
018800161125     C/EXEC SQL
018900170104     C+ FETCH NEXT FROM A1 INTO :societa, :titas, :VOLsql, :PESsql, :LINsql
019000161125     C/END-EXEC
019100161214      **                    x errori
019200161214      * Forzo la stampa del JOBLOG.
019300170213     C                   If        SqlCod < 0
019400161214     C                   CALL      'X66CHGJOB'
019500161214     C                   RETURN
019600161214     C                   End
019700161214     C*  CICLO EOF                                           0°LIVELLO
019800161214     C                   DOW       SqlCod = 0
019900161214     c                   eval            depsoc = societa
020000161214     C                   EXSR      NEWSOC
020100161214      ***
020200161214     C* CICLO ROTTURA SOCIETA                                1°LIVELLO
020300161214     C                   DOW       SqlCod = 0 and DEPSoc = Societa
020400161214      ***  Imposta campo di confronto x 2°livello
020500161214     C                   exsr      Chiave2L
020600161214     c                   eval            savCode = DepCode
020700161214      ***
020800161214     C                   EXSR      NEWCODE
020900161214      ***
021000161214     C* CICLO ROTTURA TERM.PARTENZA / LNP / LNA              2°LIVELLO
021100161214     C                   DOW       SqlCod = 0 and DEPSoc = Societa
021200161214     c                              and  savCode = DepCode
021300170215     c                   eval      almenoUno = 1
021400170215      ***
021500161207     C                   EXSR      DeTTAGLIo
021600161212      ***
021700161214     C/EXEC SQL
021800170104     C+ FETCH NEXT FROM A1 INTO :societa, :titas, :VOLsql, :PESsql, :LINsql
021900161214     C/END-EXEC
022000161214     C                   exsr      Chiave2L                                 2L
022100161125     C                   ENDDO
022200161214      **  totali x CODE                                      2°LIVELLO
022300161214     C                   EXSR      TOT_COD
022400161214     C                   ENDDO
022500161214      **                                                     0°LIVELLO
022600161214      **  totali x SOCIETA                                   1°LIVELLO
022700161214     C                   EXSR      TOT_SOC
022800161214     C                   ENDDO
022900161125     C/EXEC SQL
023000161125     C+ CLOSE A1
023100161125     C/END-EXEC
023200161214      **  totali x FINE FILE o GENERALI
023300161216     ** non si vogliono più i totali generali
023400161220     c                   if        Stampa_TGEN ='S'
023500161214     C                   EXSR      TOT_GEN
023600161214     C                   End
023700161214     c* ---------------------
023800161207     ** Chiudo il programma.
023900161207     C                   CALL      'TIBSSOCR'
024000161207     C                   PARM      'FINALIZE'    prmRqsOpCode
024100161207     C                   PARM                    prmRpyOpCode
024200161207     C                   PARM                    prmRpyIdMsg
024300161207     C*
024400161214     C                   Return
024500000000     C*------------------------------------------------------------------------*
024600161214     C* Chiave 2° LIVELLO   o TERMINAL PARTENZA o LNP o LNA
024700000000     C*------------------------------------------------------------------------*
024800161214     C     Chiave2L      BEGSR
024900161214      **
025000161214     C* Imposta la chiave specifica
025100170215      **  xchè chiamata in 3 modi diversi.
025200170215      **  nel campo DEPCODE  quindi  o TERMINAL o LINEA PART o ARR
025300170215      **
025400170116      ** Al momento si vuole raggruppato tutto sul TERMINAL
025500170116      **  (quindi serve per l'estero) come in ORDER BY aggiunto LINEA
025600170116      ** così adesso anche per eseguire le dovute rotture
025700170116     c                   eval      depcode= LINsql
025800161214      **
025900161214     C                   ENDSR
026000161214     C*------------------------------------------------------------------------*
026100161214     C* NEWCODE- OPERAZIONI PER NUOVO TERMINAL PARTENZA / LNP / LNA
026200161214     C*------------------------------------------------------------------------*
026300161214     C     NEWCODE       BEGSR
026400161214      **
026500161206     C                   Z-ADD     1             I
026600161207     C                   MOVEL     *BLANKS       DEScode
026700161207     C     DEPcode       LOOKUP    ORG(I)                                 28
026800161207     C   28              MOVEL     ODES(I)       DEScode
026900161214      **
027000161214     C* AZZERA LE VARIABILI DI LAVORO
027100161214     C                   Z-ADD     *ZEROS        TOTvolCOD        13 3
027200161214     C                   Z-ADD     *ZEROS        TOTpesCOD        13 1
027300161214     C                   Z-ADD     *ZEROS        TOTnclCOD         9 0
027400161221     C                   Z-ADD     *ZEROS        TOTspeCOD         7 0
027500990409     C*
027600000000     C                   ENDSR
027700161214     C*------------------------------------------------------------------------*
027800161214     C* NEWSOC - OPERAZIONI PER NUOVA SOCIETA'
027900161214     C*------------------------------------------------------------------------*
028000161214     C     NEWSOC        BEGSR
028100170125      **
028200161214     C* DECODIFICA la Società
028300161219     c                   eval      inputSOC = depSOC
028400161214     c                   exsr      Decod_societa
028500161219     c                   eval        desSOC = outputDES
028600161219      ***
028700161219     c                   if        partip = 'T'
028800161221     c                   eval        prtcli = inputSOC
028900161221     c                   eval        prtcliD= outputDES
029000161219     c                   else
029100161219     c                   eval        prtsoc = inputSOC
029200161219     c                   eval        prtsocD= outputDES
029300161219     c                   end
029400161219      ***
029500161219      ***  decodifica società passata fra i parametri
029600161219     c                   eval      inputSOC = parSOC
029700161219     c                   exsr      Decod_societa
029800161219     c                   if        partip = 'T'
029900161219     c                   eval        prtsoc = inputSOC
030000161219     c                   eval        prtsocD= outputDES
030100161219     c                   else
030200161221     c                   eval        prtcli = inputSOC
030300161221     c                   eval        prtcliD= outputDES
030400161219     c                   end
030500170214      **
030600170214     c*** a ogni cambio società - ri inizia a numerare le pagine
030700170214     c                   if        prtsoc <> savprtsoc
030800170214     c                   eval       savprtsoc = prtsoc
030900170214     c                   z-add     0             tecPAG
031000170214     c                   end
031100161214     C*
031200161214     C* AZZERA LE VARIABILI DI LAVORO
031300161214     C                   Z-ADD     *ZEROS        TOTVOLsoc        13 3
031400161214     C                   Z-ADD     *ZEROS        TOTpessoc        13 1
031500161214     C                   Z-ADD     *ZEROS        TOTnclsoc         9 0
031600161221     C                   Z-ADD     *ZEROS        TOTspesoc         7 0
031700161214     C                   Z-ADD     *ZEROS        RI
031800161214     C                   Z-ADD     *ZEROS        savRI
031900161214     c                   clear                   rieCOD
032000161214     c                   clear                   rieDES
032100161214     c                   clear                   rieVOL
032200161214     c                   clear                   riePES
032300161214     c                   clear                   rieNUM
032400161221     c                   clear                   rieSPE
032500161214     C*
032600161214     C                   ENDSR
032700161212     C*------------------------------------------------------------------------*
032800161214     C* TOT_COD - OPERAZIONI PER TOTALE CODICE  TERMINAL O LINEA
032900161212     C*------------------------------------------------------------------------*
033000161214     C     TOT_COD       BEGSR
033100161214     C*---
033200161214     C*  ULTIMO dettaglio
033300161214     C*   x esaurire schiere DETTAGLI
033400161214     C                   EXSR      STAMPAriga
033500161214      *--------------
033600161214     C*  DETTAGLIO
033700161214IF  2C     TOTvolCOD     IFNE      *ZEROS
033800161214     C                   Z-ADD     TOTvolCOD     TOCvol
033900161214     C                   Z-ADD     TOTpesCOD     TOCpes
034000161214     C                   Z-ADD     TOTnclCOD     TOCncl
034100161221     C                   Z-ADD     TOTspeCOD     TOCspe
034200161214      *
034300161214     c                   if        partip ='T'
034400161220     C                   MOVEL     STM(11)       TODDES
034500161214     c                   elseif    partip ='P'
034600161220     C                   MOVEL     STM(12)       TODDES
034700161214     c                   elseif    partip ='A'
034800161220     C                   MOVEL     STM(13)       TODDES
034900161214     c                   end
035000161214     c                   eval      todDES = %Trim(todDES) + ' ' +
035100161214     c                             %editc(savCODE:'X') + ' ' + DEScode
035200161214     C*
035300161214     C                   EXSR      testaDETT
035400161219     c                   if        stampa_dett ='S'
035500170117     c                             AND parSTP  ='S'
035600170110     C                   WRITE     DEEC28SE
035700161214     C                   WRITE     DEEC28TO
035800161219     c                   end
035900161214      *--------------
036000161214     C*  RIEPILOGO
036100161214     C                   Z-ADD     TOTvolCOD     DECvol
036200161214     C                   Z-ADD     TOTpesCOD     DECPes
036300161214     C                   Z-ADD     TOTnclCOD     DECncl
036400161221     C                   Z-ADD     TOTspeCOD     DECspe
036500161214     C                   MOVEL     SAVcode       DECcode
036600161214     C                   MOVEL     DEScode       DEDcode
036700161214      *
036800161214      *--- salva su schiera per riepilogare in dettaglio a TOT Società
036900161214     c                   add       1             ri
037000161214     c                   z-add     decCODE       rieCOD(RI)
037100161214     c                   movel     dedCODE       rieDES(RI)
037200161214     c                   z-add     TOTvolCOD     rieVOL(RI)
037300161214     c                   z-add     TOTpesCOD     riePES(RI)
037400161214     c                   z-add     TOTnclCOD     rieNUM(RI)
037500161221     c                   z-add     TOTspeCOD     rieSPE(RI)
037600161214     C                   Z-ADD     RI            savRI
037700161214      *---
037800161214     C                   EXSR      testaRIEP
037900161219     c                   if        stampa_riep ='S'
038000170117     c                             AND parSTP  ='S'
038100161214     C                   WRITE     RIEC27DE
038200161219     c                   end
038300161214E   2C                   ENDIF
038400161214     C*
038500161214     c                   seton                                        01
038600161214     C*
038700161214     C                   ENDSR
038800161207     C*------------------------------------------------------------------------*
038900161214     C*  - TOTALI per SOCIETA
039000161207     C*------------------------------------------------------------------------*
039100161214     C     TOT_SOC       BEGSR
039200161215     C*
039300161215IF  2C     TOTvolSOC     IFNE      *ZEROS
039400161220     C                   move      'TOTALE '     TODtot
039500161215     C                   Z-ADD     TOTvolSOC     TOCvol
039600161215     C                   Z-ADD     TOTpesSOC     TOCpes
039700161215     C                   Z-ADD     TOTnclSOC     TOCncl
039800161221     C                   Z-ADD     TOTspeSOC     TOCspe
039900161215     C* Riepilogo
040000161215     C                   EXSR      testaRIEP
040100161219     c                   if        stampa_riep ='S'
040200170117     c                             AND parSTP  ='S'
040300161215     C                   WRITE     RIEC27TO
040400161219     c                   end
040500161215     C* Dettaglio ------------
040600161219     c                   if        stampa_dett ='S'
040700170117     c                             AND parSTP  ='S'
040800161220     c                   seton                                        55
040900170116     c                   add       1             tecPAG
041000161215     C                   WRITE     DEEC28TE
041100161219     C                   WRITE     DEEC28SO
041200161215     C                   WRITE     DEEC28I3
041300161220     c                   setoff                                       55
041400161219     c                   end
041500161215      * ciclo riepilogo totali CODE
041600161215     c                   do        savri         ri
041700161215     c                   z-add     rieCOD(RI)    DE3CODE
041800161215     c                   movel     rieDES(RI)    DE3CODEd
041900161215     c                   z-add     rieVOL(RI)    DE3VOL
042000161215     c                   z-add     riePES(RI)    DE3PES
042100161215     c                   z-add     rieNUM(RI)    DE3NCL
042200161221     c                   z-add     rieSPE(RI)    DE3SPE
042300161219     c                   exsr      WRI_TNFIG
042400161219     c                   if        stampa_dett ='S'
042500170117     c                             AND parSTP  ='S'
042600161215     C                   WRITE     DEEC28D3
042700161219     c                   end
042800161215     c                   enddo
042900161215      **  totale società
043000161220     c                   eval      todDES = *blank
043100161219     c                   if        stampa_dett ='S'
043200170117     c                             AND parSTP  ='S'
043300170110     c                   seton                                        66
043400170110     C                   WRITE     DEEC28SE
043500161215     C                   WRITE     DEEC28TO
043600170110     c                   setoff                                       66
043700161219     c                   end
043800161215E   2C                   ENDIF
043900161215     C*
044000161215     c                   seton                                        0111
044100161214     C*
044200161214     C                   ENDSR
044300161214     C*------------------------------------------------------------------------*
044400161214     C*  - TOTALE GENERALE
044500161214     C*------------------------------------------------------------------------*
044600161214     C     TOT_GEN       BEGSR
044700161214     C*
044800161214     C* STAMPA RIMANENZE BOLLE E TOTALI TFP/TFA -DETTAGLIO E RIEPILOGO-
044900161214     c                   if        almenoUno = 1
045000161214      **
045100161214     C                   Z-ADD     1             S
045200161214     C     S             OCCUR     BDAT
045300161214     C*
045400161214     C* STAMPA IL TOTALE GENERALE E IL FINE ALLEGATO
045500161214     C                   EXSR      STotGEN
045600161214     C                   EXSR      STAFINE
045700161214     C*
045800161214     C* STAMPA LA SINTESI DELL'ELABORAZIONE
045900161219     c                   open      qsysprt
046000161214     c                   except    tes
046100161214     c                   except    det
046200161219     c                   close     qsysprt
046300161214     c                   end
046400161214     C*
046500161214     C                   ENDSR
046600161214     C*------------------------------------------------------------------------*
046700161214     C*  - MEMORIZZA E STAMPA DATI BOLLE
046800161214     C*------------------------------------------------------------------------*
046900161214     C     DEttagliO     BEGSR
047000000000     C*---
047100000000     C* CONTROLLA QUALE SPOOL CONSIDERARE
047200100107     C                   Z-ADD     1             S                 1 0
047300100107     C                   ADD       1             B1                1 0
047400100108     C                   Z-ADD     B1            B                 1 0
047500990419     C*
047600000000     C* MEMORIZZA I DATI DELLA BOLLA
047700990419     C     S             OCCUR     BDAT
047800161128     C                   Z-ADD     tasLNP        LNP(B)
047900161128     C                   Z-ADD     tasNRS        NRS(B)
048000161221     C                   Z-ADD     tasNSP        NSP(B)
048100161128     C                   Z-ADD     tasMGS        MGS(B)
048200161207     C                   Z-ADD     tasncl        Snc(B)
048300161221     C                   Z-ADD     1             SPE(B)
048400161212     C                   Z-ADD     VOLsql        VOL(B)
048500161206      *
048600161128     c* il volume è già risolto nell'istruzione sql mentre il peso no
048700161128     c* perchè ha più casistiche
048800161212     c                   if        PESsql <> 0
048900161212     C                   Z-ADD     PESsql        Spk(B)
049000161207     c                   else
049100161128     c                   if        tasfpf = 'V'
049200161128     C                   Z-ADD     taspkc        Spk(B)
049300161128     c                   else
049400161128     C                   Z-ADD     taspkf        Spk(B)
049500161128     c                   end
049600161128     c                   end
049700000000     C*
049800000000     C* CONTROLLA SE STAMPARE IL BLOCCO DELLE BOLLE MEMORIZZATE
049900000000IF  1C     S             IFEQ      1
050000161214     C                   ADD       VOLsql        TOTvolCOD
050100161212     C                   ADD       VOLsql        TOTVOLsoc
050200161214     C                   ADD       VOLsql        TOTVOLgen
050300161206      *
050400161214     C                   add       tasncl        totnclCOD
050500161207     C                   add       tasncl        totnclsoc
050600161214     C                   add       tasncl        totnclgen
050700161207      *
050800161221     C                   add       1             totspeCOD
050900161221     C                   add       1             totspesoc
051000161221     C                   add       1             totspegen
051100161221      *
051200161212     c                   if        PESsql <> 0
051300161212     C                   add       PESsql        totpessoc
051400161214     C                   add       PESsql        totpesCOD
051500161214     C                   add       PESsql        totpesGEN
051600161214      *
051700161207     c                   else
051800161207      *
051900161128     c                   if        tasfpf = 'V'
052000161214     C                   add       taspkc        totpesCOD
052100161214     C                   add       taspkc        totpesSOC
052200161214     C                   add       taspkc        totpesGEN
052300161128     c                   else
052400161214     C                   add       taspkf        totpesCOD
052500161214     C                   add       taspkf        totpesSOC
052600161214     C                   add       taspkf        totpesGEN
052700161128     c                   end
052800161128     c                   end
052900161206      *
053000161207     c* per traini sono 7 bolle su una riga , mentre per distribuz. 4
053100161128     c                   if        (partip = 'T' and B1 = 7) or
053200161212     c                             (partip <>'T' and B1 = 4)
053300161214     C                   EXSR      STAMPAriga
053400000000E   2C                   ENDIF
053500161206      *
053600000000E   1C                   ENDIF
053700161206      *
053800100107     C                   ENDSR
053900100107     C*------------------------------------------------------------------------*
054000161219     C* WRI_TNFIG- scrive wf per memorizzare i mc per terminal partenza
054100100107     C*------------------------------------------------------------------------*
054200161219     C     WRI_TNFIG     BEGSR
054300161207     C*
054400161221     c                   movel     prtcli        FIgcli
054500161221     c                   movel     prtclid       FIgcliD
054600161219     c                   movel     prtSOC        FIgSOC
054700161219     c                   movel     prtSOCd       FIgSOCD
054800161216     C                   Z-ADD     PARANN        FIgANNO
054900161216     C                   Z-ADD     PARMES        FIgMESE
055000161216     c                   movel     PARSIM        FIgSIM
055100161216     C                   movel     parTIP        FIgTIP
055200161216     C                   movel     DE3CODE       FIgTFP
055300161216     C                   movel     DE3CODEd      FIgTFPD
055400161216     C                   Z-ADD     DE3VOL        FIgVMC
055500161216     C                   Z-ADD     DE3PES        FIgPKG
055600161216     C                   Z-ADD     DE3NCL        FIgNCL
055700161221     C                   Z-ADD     DE3SPE        FIgSPE
055800161219     C                   WRITE     TNFIG000
055900100107     C*
056000000000     C                   ENDSR
056100000000     C*------------------------------------------------------------------------*
056200161214     C* STAMPAriga  - STAMPA IL BLOCCO DELLE BOLLE MEMORIZZATE
056300000000     C*------------------------------------------------------------------------*
056400161214     C     STAMPAriga    BEGSR
056500000000     C*
056600990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
056700000000IF  1C     LNP(1)        IFGT      *ZEROS
056800000000     C                   Z-ADD     LNP(1)        DECLN1
056900000000     C                   Z-ADD     NRS(1)        DECNR1
057000000000     C                   Z-ADD     NSP(1)        DECNS1
057100000000     C                   Z-ADD     MGS(1)        DECMG1
057200161212     C                   Z-ADD     VOL(1)        DECSM1
057300161207     C                   Z-ADD     SNC(1)        DECSC1
057400161207     C                   Z-ADD     SPK(1)        DECSP1
057500161207      *
057600000000     C                   Z-ADD     LNP(2)        DECLN2
057700000000     C                   Z-ADD     NRS(2)        DECNR2
057800000000     C                   Z-ADD     NSP(2)        DECNS2
057900000000     C                   Z-ADD     MGS(2)        DECMG2
058000161212     C                   Z-ADD     VOL(2)        DECSM2
058100161207     C                   Z-ADD     SNC(2)        DECSC2
058200161207     C                   Z-ADD     SPK(2)        DECSP2
058300161207      *
058400000000     C                   Z-ADD     LNP(3)        DECLN3
058500000000     C                   Z-ADD     NRS(3)        DECNR3
058600000000     C                   Z-ADD     NSP(3)        DECNS3
058700000000     C                   Z-ADD     MGS(3)        DECMG3
058800161212     C                   Z-ADD     VOL(3)        DECSM3
058900161207     C                   Z-ADD     SNC(3)        DECSC3
059000161207     C                   Z-ADD     SPK(3)        DECSP3
059100161207      *
059200000000     C                   Z-ADD     LNP(4)        DECLN4
059300000000     C                   Z-ADD     NRS(4)        DECNR4
059400000000     C                   Z-ADD     NSP(4)        DECNS4
059500000000     C                   Z-ADD     MGS(4)        DECMG4
059600161212     C                   Z-ADD     VOL(4)        DECSM4
059700161207     C                   Z-ADD     SNC(4)        DECSC4
059800161207     C                   Z-ADD     SPK(4)        DECSP4
059900161207      *
060000000000     C                   Z-ADD     LNP(5)        DECLN5
060100000000     C                   Z-ADD     NRS(5)        DECNR5
060200000000     C                   Z-ADD     NSP(5)        DECNS5
060300000000     C                   Z-ADD     MGS(5)        DECMG5
060400161212     C                   Z-ADD     VOL(5)        DECSM5
060500161207      *
060600000000     C                   Z-ADD     LNP(6)        DECLN6
060700000000     C                   Z-ADD     NRS(6)        DECNR6
060800000000     C                   Z-ADD     NSP(6)        DECNS6
060900000000     C                   Z-ADD     MGS(6)        DECMG6
061000161212     C                   Z-ADD     VOL(6)        DECSM6
061100161207      *
061200000000     C                   Z-ADD     LNP(7)        DECLN7
061300000000     C                   Z-ADD     NRS(7)        DECNR7
061400000000     C                   Z-ADD     NSP(7)        DECNS7
061500000000     C                   Z-ADD     MGS(7)        DECMG7
061600161212     C                   Z-ADD     VOL(7)        DECSM7
061700000000     C* FATTURE BAR
061800000000IF  2C     S             IFEQ      1
061900161213     C                   EXSR      testaDETT
062000161219     c                   if        stampa_dett ='S'
062100170117     c                             AND parSTP  ='S'
062200161214     C  n44              WRITE     DEEC28D1
062300161214     C   44              WRITE     DEEC28D2
062400161219     c                   end
062500000000     C                   Z-ADD     *ZEROS        B1
062600000000E   2C                   ENDIF
062700000000     C*
062800000000     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
062900161214     C                   CLEAR                   LNP
063000161214     C                   CLEAR                   NRS
063100161214     C                   CLEAR                   NSP
063200161214     C                   CLEAR                   MGS
063300161214     C                   CLEAR                   VOL
063400161214     C                   CLEAR                   Spk
063500161214     C                   CLEAR                   Snc
063600161221     C                   CLEAR                   SPE
063700161214     C*
063800000000E   1C                   ENDIF
063900000000     C*
064000000000     C                   ENDSR
064100990412     C*------------------------------------------------------------------------*
064200161213     C* STotGEN- STAMPA TOTALE GENERALE -DETTAGLIO-
064300000000     C*------------------------------------------------------------------------*
064400161213     C     STotGEN       BEGSR
064500000000     C*
064600161213     C* FATTURE BAR
064700161214IF  1C     TOTVOLgen     IFNE      *ZEROS
064800161213     C* Tot.Generale
064900161212     C                   MOVEL     STM(3)        TODDES
065000161213     C                   MOVEL     STM(3)        TODTOT
065100161220     C                   MOVE      'TOTALE'      TODTOT
065200161214     C                   Z-ADD     TOTvolGEN     TOCvol
065300161212     C*
065400161219     c                   if        stampa_dett ='S'
065500170117     c                             AND parSTP  ='S'
065600170116     c                   add       1             tecPAG
065700161214     C                   WRITE     DEEC28TE
065800170110     C                   WRITE     DEEC28SE
065900161214     C                   WRITE     DEEC28TO
066000161219     c                   end
066100161213     C*
066200161213     C*--- RIEPILOGO -------------------------
066300161213     c                   seton                                        33
066400161213     C                   EXSR      testaRIEP
066500161219     c                   if        stampa_riep ='S'
066600170117     c                             AND parSTP  ='S'
066700161214     C                   WRITE     RIEC27TO
066800161219     c                   end
066900161212E   1C                   ENDIF
067000000000     C*
067100000000     C                   ENDSR
067200000000     C*------------------------------------------------------------------------*
067300161213     C* testaDETT - STAMPA TESTATA DETTAGLIO
067400000000     C*------------------------------------------------------------------------*
067500161213     C     testaDETT     BEGSR
067600000000     C*
067700000000IF  1C     *IN01         IFEQ      *ON
067800161213      *---
067900161219     c                   if        stampa_dett ='S'
068000170117     c                             AND parSTP  ='S'
068100170116     c                   add       1             tecPAG
068200161214     C                   WRITE     DEEC28TE
068300161214     C                   WRITE     DEEC28SO
068400161214     C  n44              WRITE     DEEC28I1
068500161214     C   44              WRITE     DEEC28I2
068600161219     c                   end
068700000000     C                   MOVEL     '0'           *IN01
068800000000E   1C                   ENDIF
068900000000     C*
069000000000     C                   ENDSR
069100000000     C*------------------------------------------------------------------------*
069200161213     C* testaRIEP      - STAMPA TESTATA RIEPILOGO
069300000000     C*------------------------------------------------------------------------*
069400161213     C     testaRIEP     BEGSR
069500000000     C*
069600000000IF  1C     *IN11         IFEQ      *ON
069700161213      *
069800161219     c                   if        stampa_riep ='S'
069900170117     c                             AND parSTP  ='S'
070000161214     C                   WRITE     RIEC27TE
070100161214     C  N33              WRITE     RIEC27SO
070200161219     C                   WRITE     RIEC27IN
070300161219     c                   end
070400000000     C                   MOVEL     '0'           *IN11
070500000000E   1C                   ENDIF
070600000000     C*
070700000000     C                   ENDSR
070800000000     C*------------------------------------------------------------------------*
070900000000     C* *INZSR - OPERAZIONI INIZIALI
071000000000     C*------------------------------------------------------------------------*
071100000000     C     *INZSR        BEGSR
071200000000     C*---
071300000000     C* RICEVIMENTO PARAMETRI
071400000000     C     *ENTRY        PLIST
071500000000     C                   PARM                    KPJBA
071600000000     C                   MOVEL     KPJBU         PARAM
071700161221      **
071800161221     C     KEYTAB        KLIST
071900161221     C                   KFLD                    TBLKUT
072000161221     C                   KFLD                    TBLCOD
072100161221     C* Completa
072200161221     C     KTAB          KLIST
072300161221     C                   KFLD                    TBLKUT
072400161221     C                   KFLD                    TBLCOD
072500161221     C                   KFLD                    TBLKEY
072600161206      **
072700100108     C* REPERISCE LA DATA CORRENTE
072800100108     C                   TIME                    N14              14 0
072900100108     C                   MOVE      N14           DATA              8 0
073000100108     C                   MOVEL     N14           ORA               6 0
073100161213     C                   MOVEL     *all'_'       separa
073200161219      **-
073300161219     c                   if        stampa_dett = 'S'
073400161219     c                   open      TREC28DP
073500161219     c                   end
073600161219     c                   if        stampa_riep = 'S'
073700161219     c                   open      TREC28RP
073800161219     c                   end
073900161213      **-
074000161216     c                   if        partip ='T'
074100161216     C                   MOVEL     STM(3)        TECTES
074200161216     C                   MOVEL     STM(7)        campo25          25
074300161220     C                   MOVEL     STM(11)       IN3DES
074400161220     c                   evalr     tiposta = %Trim(STM(11))
074500161216     c                   elseif    partip ='P'
074600161216     C                   MOVEL     STM(4)        TECTES
074700161216     C                   MOVEL     STM(8)        campo25
074800161220     C                   MOVEL     STM(12)       IN3DES
074900161220     c                   evalr     tiposta = %Trim(STM(12))
075000161216     c                   elseif    partip ='A'
075100161216     C                   MOVEL     STM(4)        TECTES
075200161216     C                   MOVEL     STM(9)        campo25
075300161220     C                   MOVEL     STM(13)       IN3DES
075400161220     c                   evalr     tiposta = %Trim(STM(13))
075500161216     c                   end
075600161216      **-
075700161214     c* pulisce il periodo prima di aprire il file
075800161214     C/EXEC SQL
075900161219     C+ DELETE  FROM TNfig00f  WHERE  FigSIM = :PARSIM and
076000161216     C+   FIgANNO = :PARANN  and FIgMESE = :PARMES
076100161216     C+                      and FIgTIP  = :PARTIP
076200161214     C/END-EXEC
076300161219     c                   open      TNFIG00F
076400161216      **-
076500161213     c                   eval      tecTES = %trim(tecTES) +
076600161220     c                             '  ' + campo25
076700161220     c                   eval      tecTES = %trim(tecTES) +
076800161220     c                             '  PERIODO ' + %editc(parMES:'X') +
076900161220     c                             '/' + %editc(parANN:'X')
077000161220      ****
077100161214     C                   MOVEL     campo25       INDDES
077200000000     C*--------------------
077300000000     C* VARIABILI DI CONTROLLO
077400000000     C*--------------------
077500000000     C                   MOVEL     'N'           $FINE             1
077600000000     C                   MOVEL     'N'           WRECOK            1
077700000000     C                   MOVEL     '1'           *IN01
077800000000     C                   MOVEL     '1'           *IN11
077900161213     C                   MOVEL     '0'           *IN44
078000161213     C*
078100161212     c                   if        partip <>'T'
078200161213     C                   MOVEL     '1'           *IN44
078300161212     c                   end
078400000000     C*
078500000000     C* REPERISCE LA DATA CORRENTE
078600000000     C                   TIME                    N14              14 0
078700000000     C                   MOVE      N14           N8                8 0
078800000000     C                   Z-ADD     N8            G08DAT
078900000000     C                   Z-ADD     *ZEROS        G08INV
079000000000     C                   MOVEL     '0'           G08ERR
079100000000     C                   CALL      'XSRDA8'
079200000000     C                   PARM                    WLBDA8
079300000000     C                   Z-ADD     G08INV        DATCOR            8 0
079400161216     c*---
079500161216     c* ORGANIGRAMMA
079600161216     C                   Z-ADD     *ZEROS        I                 5 0
079700161216     C     *LOVAL        SETLL     AZORG01L
079800161216     C                   READ      AZORG01L                               99
079900161216DO  1C     *IN99         DOWEQ     *OFF
080000161216IF  2C     ORGFVA        IFEQ      *BLANKS
080100161216     C     ORGFAG        ANDNE     'V'
080200161216     C                   ADD       1             I
080300161216     C                   Z-ADD     ORGFIL        ORG(I)
080400161216     C                   MOVEL     ORGDES        ODES(I)
080500161216E   2C                   ENDIF
080600161216     C                   READ      AZORG01L                               99
080700161216E   1C                   ENDDO
080800000000     C*
080900000000     C* DECODIFICA I PARAMETRI DI LANCIO
081000161212     C                   MOVEL     'TREC28R'     TECpgm
081100161212IF  1C     PARSIM        IFEQ      'S'
081200161212     C                   MOVEL     STM(10)       TEDSIM
081300161212X   1C                   ELSE
081400161212     C                   MOVEL     *BLANKS       TEDSIM
081500161212E   1C                   ENDIF
081600161221     C*---
081700161221     C* TIPOLOGIA TIPI BOLLE
081800161221     c                   clear                   W2                3 0
081900161221     c                   clear                   FSP2WS
082000161221     C                   Z-ADD     1             TBLKUT
082100161221     C                   MOVEL     '2W'          TBLCOD
082200161221     C     KEYTAB        SETLL     TABEL00F
082300161221     C     KEYTAB        READE(n)  TABEL00F
082400161221     c                   dow       not %EoF(Tabel00F)
082500161221     c                   movel     tblUNI        ds2W
082600161221     c                   if        §2WFSP='S'
082700161221     c                   add       1             W2
082800161221     c                   movel     tblkey        FSP2WS(W2)
082900161221     c                   end
083000161221     C     KEYTAB        READE(n)  TABEL00F
083100161221     c                   enddo
083200161221     C*---
083300161221     C* TIPI BOLLE DA CONSIDERARE
083400161221     C                   Z-ADD     1             TBLKUT
083500161221     C                   MOVEL     'TB'          TBLCOD
083600161221     c                   clear                   Tipi_Bolla
083700161221     C     KEYTAB        SETLL     TABEL00F
083800161221     C     KEYTAB        READE(n)  TABEL00F
083900161221     c                   dow       not %EoF(Tabel00F)
084000161221     c                   movel     tblUNI        dsTB
084100161221      * sulla precedente tabella (a Spedizione)
084200161221     c     §TBRBL        lookup    FSP2WS                                 21
084300161221     c                   if        *in21
084400161221     c                   eval      Tipi_Bolla = %trim(Tipi_Bolla) +
084500161221     c                             ' ''' + %trim(tblkey) + ''', '
084600161221     c                   end
084700161221     C     KEYTAB        READE(n)  TABEL00F
084800161221     c                   enddo
084900161221     c                   clear                   Lung              3 0
085000161221      *
085100161221      *  Quanti caratteri ha inserito nel campo ?
085200161221     c                   eval      Lung = %Len(%Trim(Tipi_Bolla))
085300161221      * ultima virgola aggiunta
085400161221      *   deve trasformarla in blank
085500161221     c                   if        Lung >0
085600161221     c                   if        %subst(Tipi_Bolla:Lung:1)=','
085700161221     c                   eval      %subst(Tipi_Bolla:Lung:1)=*blank
085800161221     c                   end
085900161221     c                   end
086000161221     C*---
086100161221     C* NUMERATORE ALLEGATI FATTURAZIONE INFRAGRUPPO
086200161221     C                   Z-ADD     1             TBLKUT
086300161221     C                   MOVEL     '$5'          TBLCOD
086400161221     c                   movel     partip        campo5            5
086500161221     c                   move      parann        campo5
086600161221     c                   movel     campo5        campo7            7
086700161221     c                   move      parmes        campo7
086800161221     C                   MOVEL(P)  campo7        TBLKEY
086900161221     C                   CLEAR                   DS$5
087000161221     C     KTAB          CHAIN     TABEL00F
087100161221IF  1C                   IF        %Found(Tabel00F)
087200161221     C                   MOVEL     TBLUNI        DS$5
087300161221E   1C                   ENDIF
087400161221     C*---
087500161221     C* CALCOLA IL NUMERATORE PER TUTTI GLI ALLEGATI
087600161221     c                   if        stampa_dett = 'S'
087700161221     C     §$5NUA        ADD       1             contaD
087800161221     C                   Z-ADD     contaD        NEWNUA
087900161221     c                   end
088000161221      *
088100161221     c                   if        stampa_riep = 'S'
088200161221     C     contaD        ADD       1             contaR
088300161221     C                   Z-ADD     contaR        NEWNUA
088400161221     c                   end
088500161221      *
088600161221     C*
088700161221     C* AGGIORNA IL NUMERATORE
088800161221IF  1C     PARSIM        IFEQ      'N'
088900161221IF  1C                   IF        %Found(Tabel00F)
089000161221     C                   Z-ADD     NEWNUA        §$5NUA
089100161221     C                   MOVEL     DS$5          TBLUNI
089200161221     C                   UPDATE    TABEL
089300161221X   2C                   ELSE
089400161221     C                   Z-ADD     1             TBLKUT
089500161221     C                   MOVEL     '$5'          TBLCOD
089600161221     c                   movel     partip        campo5
089700161221     c                   move      parann        campo5
089800161221     c                   movel     campo5        campo7            7
089900161221     c                   move      parmes        campo7
090000161221     C                   MOVEL(p)  campo7        TBLKEY
090100161221     C                   Z-ADD     NEWNUA        §$5NUA
090200161221     C                   eval         TBLUNI = DS$5
090300161221     C                   WRITE     TABEL
090400161221E   2C                   ENDIF
090500161221X   1C                   ELSE
090600161221     C     KTAB          SETGT     TABEL00F
090700161221E   1C                   ENDIF
090800161221     C*
090900000000     C                   ENDSR
091000161206     C*--------------------------------------------------------------**
091100161213     C*   Decodifica Società
091200161213     C*--------------------------------------------------------------**
091300161207     C     Decod_societa begSR
091400161219     C                   clear                   outputDES
091500161207      *
091600161207     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
091700161207     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
091800161207     C                   RESET                   tibsSocI0
091900161219     C                   EVAL      tibsSocI0.idSocieta = inputSOC
092000161207     C
092100161207     C                   CALL      'TIBSSOCR'
092200161207     C                   PARM      'GETANAGRAF'  prmRqsOpCode
092300161207     C                   PARM      *BLANK        prmRpyOpCode
092400161207     C                   PARM      *ZERO         prmRpyIdMsg
092500161207     C                   PARM      'TIBSSOCI0'   prmRqsFormato
092600161207     C                   PARM                    tibsSocI0
092700161207     C                   PARM                    prmRqsDataSize
092800161207     C                   PARM      'TIBSSOCO0'   prmRpyFormato
092900161207     C                   PARM                    tibsSocO0
093000161207     C                   PARM                    prmRpyDataSize
093100161207     c                   if         PRMRPYIDMSG >= 0
093200161207     c                              and TIBSSOCO0.IDSOCIETA <> *blank
093300161219     c                   eval      outputDES = tibsSocO0.RAGSOCIALE
093400161207     c                   else
093500161219     c                   eval      outputDES = 'Errore'
093600161207     c                   end
093700161207      *
093800161207     C                   ENDSR
093900161212?     *--------------------------------------------------------------*
094000161212?     *  istruz: Operazioni di inizializzazione dati                 *
094100161212?     *--------------------------------------------------------------*
094200161212     C     istruz        BEGSR
094300161212     **
094400170215     **      3 CASI : come viene sviluppato l'SQL sotto:
094500170216     **
094600170216?     *-------------------------------------
094700170215     **  x TERMINAL PARTENZA
094800170224     **       attenzione CHIODO x FILIALE 172 Società 257 deve apparire 201
094900170215?     *-------------------------------------
095000170215     **  WITH
095100170215     **  LINEEITA  as
095200170215     **  (SELECT orgfil FROM AZORG00F  WHERE
095300170215     **  substr(orgde3, 11, 3) in  (SELECT substr(tbeke1, 1, 3) FROM
095400170215     **  tntbe00f WHERE tbecod='NTW' and substr(tbeuni, 31, 1) <>'E')
095500170215     **    and  orgfag <> 'V'
095600170215     **   ),
095700170215     **  SPEDIZIONI as   (
095800170215     **  (SELECT TITAS10F.*  ,
095900170215     **  CASE WHEN dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
096000170215     **  is null THEN
096100170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
096200170215     **  END AS VOL,
096300170215     **  CASE WHEN dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1)
096400170215     **  is null THEN 0 ELSE
096500170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
096600170215     **  tasTFP as LINEA
096700170215     **  FROM  TITAS10F left outer JOIN FIAR500F
096800170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
096900170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
097000170215     **  left outer JOIN  LINEEITA  on tasTFP = orgfil
097100170215     **  WHERE
097200170215     **   tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02 and
097300170215     **  tastbl in ('AS', 'A2', 'FS', 'F1')    )
097400170215     **      UNION
097500170215     **  (SELECT TITASP0F.* ,
097600170215     **  CASE WHEN dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
097700170215     **   is null THEN tasvlf ELSE
097800170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) END AS VOL,
097900170215     **  CASE WHEN dec(dec(substr(ar5uni, 2,  7), 7)/10, 7, 1)
098000170215     **  is null THEN 0 ELSE
098100170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
098200170215     **  tasTFP as LINEA
098300170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
098400170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
098500170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
098600170215     **  left outer JOIN  LINEEITA  on tasTFP = orgfil
098700170215     **  WHERE
098800170215     **  tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02 and
098900170215     **  tastbl in ('AS', 'A2', 'FS', 'F1')     )
099000170215     **  )
099100170215     **
099200170224     **  SELECT * FROM (
099300170224     **  SELECT case when ID_SOCIETA ='257' and ID_FILIALE = 172
099400170224     **  then '201' else ID_SOCIETA END as SOCCLI,
099500170224     **  SPEDIZIONI.* FROM
099600170215     **  SPEDIZIONI join AZSOF00F on LINEA = ID_FILIALE WHERE
099700170215     **  VALIDITA_DATA_INIZIO <=
099800170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
099900170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
100000170215     **  VALIDITA_DATA_FINE >=
100100170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
100200170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)
100300170224     **  and id_societa <> '219' and tastfp <> tastfa) FILE
100400170224     **  ORDER BY
100500170224     **  SOCCLI , LINEA , tastfp, tasaas , tasmgs, tasLNP
100600170215     **
100700170215?     *--------------------------------------------------------------
100800170215     **  x LINEA ARRIVO
100900170215?     *-------------------------------------
101000170215     **  WITH
101100170215     **  LINEEITA  as (
101200170215     **  SELECT orgfil FROM AZORG00F WHERE  orgfag <> 'V'  and
101300170215     **  substr(orgde3, 11, 3) IN  (SELECT substr(tbeke1, 1, 3)
101400170215     **  FROM  tntbe00f WHERE tbecod='NTW' and
101500170215     **  substr(tbeuni, 31, 1) <>'E')
101600170215     **  ),
101700170215     **  SPEDIZIONI as (
101800170215     **  (SELECT TITAS10F.*, CASE WHEN
101900170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
102000170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
102100170215     **  END AS VOL, CASE WHEN
102200170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
102300170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
102400170215     **  CASE WHEN orgfil is null
102500170215     **  THEN tasTFA  ELSE tasLNA END as LINEA
102600170215     **  FROM  TITAS10F left outer JOIN FIAR500F
102700170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp
102800170215     **  and ar5nrs =  tasnrs and ar5nsp = tasnsp
102900170215     **  left outer JOIN  LINEEITA  on tasLNA = orgfil
103000170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
103100170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
103200170215     **  UNION
103300170215     **  (SELECT TITASP0F.*, CASE WHEN
103400170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
103500170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
103600170215     **  END AS VOL, CASE WHEN
103700170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
103800170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
103900170215     **  CASE WHEN  orgfil is null
104000170215     **  THEN tasTFA  ELSE tasLNA END as LINEA
104100170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
104200170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp
104300170215     **  and ar5nrs =  tasnrs and ar5nsp = tasnsp
104400170215     **  left outer JOIN  LINEEITA  on tasLNA = orgfil
104500170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
104600170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
104700170215     **  )
104800170215     **
104900170215     **  SELECT ID_SOCIETA, SPEDIZIONI.*
105000170215     **  FROM SPEDIZIONI join AZSOF00F
105100170215     **  on LINEA = ID_FILIALE
105200170215     **  WHERE  VALIDITA_DATA_INIZIO <=
105300170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
105400170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
105500170215     **  VALIDITA_DATA_FINE >=
105600170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
105700170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)   and
105800170215     **  id_societa <> '201'
105900170215     **  ORDER BY ID_SOCIETA , LINEA , taslna, tasaas , tasmgs
106000170215     **
106100170215?     *--------------------------------------------------------------
106200170215     **  x LINEA PARTENZA
106300170215?     *-------------------------------------
106400170215     **  WITH
106500170215     **  LINEEITA  as (
106600170215     **  SELECT orgfil FROM AZORG00F WHERE  orgfag <> 'V'  and
106700170215     **  substr(orgde3, 11, 3) IN  (SELECT substr(tbeke1, 1, 3)
106800170215     **  FROM  tntbe00f
106900170215     **  WHERE tbecod='NTW' and  substr(tbeuni, 31, 1) <>'E')
107000170215     **  ),
107100170215     **  SPEDIZIONI as (
107200170215     **  (SELECT TITAS10F.*, CASE WHEN
107300170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
107400170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
107500170215     **  END AS VOL, CASE WHEN
107600170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
107700170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
107800170215     **  CASE WHEN  orgfil is null
107900170215     **  THEN tasTFP  ELSE tasLNP END as LINEA
108000170215     **  FROM  TITAS10F left outer JOIN FIAR500F
108100170215     **  ON ar5trd='FAT' and ar5aas = tasaas and ar5lnp = taslnp and
108200170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
108300170215     **  left outer JOIN  LINEEITA  on tasLNP = orgfil
108400170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
108500170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
108600170215     **  UNION
108700170215     **  (SELECT TITASP0F.* , CASE WHEN
108800170215     **  dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3) is null THEN
108900170215     **  tasvlf ELSE dec(dec(substr(ar5uni, 10, 5), 5)/1000, 5, 3)
109000170215     **  END AS VOL, CASE WHEN
109100170215     **  dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) is null THEN 0
109200170215     **  ELSE dec(dec(substr(ar5uni, 2, 7), 7)/10, 7, 1) END AS PES,
109300170215     **  CASE WHEN  orgfil is null
109400170215     **  THEN tasTFP  ELSE tasLNP END as LINEA
109500170215     **  FROM  TITASP0F left outer JOIN FIAR5P0F
109600170215     **  ON ar5trd='FAT' and ar5aas = tasaas  and ar5lnp = taslnp and
109700170215     **  ar5nrs =  tasnrs and ar5nsp = tasnsp
109800170215     **  left outer JOIN  LINEEITA  on tasLNP = orgfil
109900170215     **  WHERE tasaas = 2016 and substr(digits(tasmgs), 1, 2) = 02
110000170215     **  and tastbl in ('AS', 'A2', 'FS', 'F1') )
110100170215     **   )
110200170215     **
110300170215     **  SELECT ID_SOCIETA, SPEDIZIONI.* FROM
110400170215     **  SPEDIZIONI join AZSOF00F on LINEA = ID_FILIALE
110500170215     **  WHERE  VALIDITA_DATA_INIZIO <=
110600170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
110700170215     **  DIGITS(tasmgs)), 'YYYYMMDD') ) , iso) and
110800170215     **  VALIDITA_DATA_FINE >=
110900170215     **  varchar(DATE(TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas),
111000170215     **  DIGITS(tasmgs)), 'YYYYMMDD')), iso)   and
111100170215     **  id_societa <> '201'
111200170215     **  ORDER BY ID_SOCIETA , LINEA , taslnp, tasaas , tasmgs
111300170215?     *--------------------------------------------------------------
111400170215     **  L'istruzione SQL si compone di una parte preparatoria dei DATI
111500170215     **   con la WITH.
111600170215     **    il primo file : LINEEITA   (solo le LINEE ITALIA no ESTERO)
111700170215     **    il secondo    : SPEDIZIONI (da TITAS10F e TITASP0F con i loro AR5)
111800170216     ** ----------------------
111900170216     **  In funzione del tipo di lancio del programma:
112000170216     **  Terminal / Linea Partenza / Linea Arrivo
112100170216     **  imposto le parti variabili necessarie all'SQL
112200170216     ** ----------------------
112300170216      *  TIPO serve per lancio Terminal in modo da escludere
112400170216      *  le spedizoni senza una vero viaggio di linea
112500170216      *  CAMPO serve per ordinare nell'SQL.
112600170216     c                   clear                   tipo
112700170216     c                   clear                   campo
112800161212     c                   select
112900170215     c                   when      partip = 'T'
113000161213     c                   eval      tipo = ' and tastfp <> tastfa '
113100170109     c                   eval      campo = 'tastfp'
113200161212     c                   when      partip = 'P'
113300161212     c                   eval      campo = 'taslnp'
113400161212     c                   when      partip = 'A'
113500161212     c                   eval      campo = 'taslna'
113600161212     c                   endsl
113700170216      *
113800170215     C                   EVAL      WrkSqlCmd ='WITH '                       +
113900170216      *  dall'Organigramma le linee con Network presente in tab."NTW"
114000170216      *  che NON sono Estere
114100170215     C                             ' LINEEITA  as ('                        +
114200170215     C                               ' SELECT orgfil FROM AZORG00F WHERE '  +
114300170215     C                             ' orgfag <> ''V''  and '                 +
114400170215     C                             ' substr(orgde3, 11, 3) IN '             +
114500170215     C                             ' (SELECT substr(tbeke1, 1, 3) FROM '    +
114600170109     C                             ' tntbe00f WHERE tbecod=''NTW'' and '    +
114700170215     C                             ' substr(tbeuni, 31, 1) <>''E'') '       +
114800170215     C                             ' ),   '                                 +
114900170216      *  LEGGE TITAS e il FIAR5 record FAT x risolvere i PESI e VOLUMI
115000170216     C                             ' SPEDIZIONI as ('                       +
115100170215     C                               ' (SELECT TITAS10F.* , CASE WHEN '     +
115200170104     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
115300170104     C                             ' 5, 3) is null THEN tasvlf ELSE '       +
115400170104     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
115500170104     C                             ' 5, 3) END AS VOL, CASE WHEN '          +
115600170104     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
115700170104     C                             ' 1) is null THEN 0 ELSE '               +
115800170104     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
115900170109     C                             ' 1) END AS PES, '
116000170216      * campo LINEA conterrà terminal o le linee partenza o arrivo a
116100170216      * seconda del tipo lancio
116200170216      * se la linea è ESTERA la sostituisce con relativo terminal
116300170109     c                   if        partip ='T'
116400170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
116500170109     C                             ' tasTFP '
116600170109     c                   elseif    partip ='P'
116700170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
116800170109     C                             ' CASE WHEN  orgfil is null THEN tasTFP '+
116900170109     C                             ' ELSE tasLNP END '
117000170109     c                   elseif    partip ='A'
117100170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
117200170109     C                             ' CASE WHEN  orgfil is null THEN tasTFA '+
117300170109     C                             ' ELSE tasLNA END '
117400170109     c                   end
117500170216      *
117600170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
117700170215     C                             ' as LINEA    FROM '                     +
117800170215     C                             ' TITAS10F left outer JOIN FIAR500F '    +
117900170215     C                             ' ON ar5trd=''FAT'' and ar5aas = tasaas '+
118000170215     C                             ' and ar5lnp = taslnp and ar5nrs = '     +
118100170215     C                             ' tasnrs and ar5nsp = tasnsp '           +
118200170215     C                             ' left outer JOIN  LINEEITA  on '
118300170216      *  in funzione del tipo lancio compone i campi chiave per
118400170216      *  joinare titas con l'oraganigramma
118500170110     c                   if        partip ='T'
118600170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
118700170215     C                             ' tasTFP = orgfil '
118800170110     c                   elseif    partip ='P'
118900170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
119000170215     C                             ' tasLNP = orgfil '
119100170110     c                   elseif    partip ='A'
119200170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
119300170215     C                             ' tasLNA = orgfil '
119400170110     c                   end
119500170216      *  nella WHERE i range dei Parametri ricevuti
119600170216      *  nel campo TIPI_BOLLA ci sono i codici bolla considerati
119700170216      *  (tab.2W) Spedizioni
119800170110     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
119900170215     C                             ' WHERE tasaas = ' + %editc(parann:'X')  +
120000170104     C                             ' and substr(digits(tasmgs), 1, 2) = '   +
120100170104     C                             %editc(parmes:'X') + ' and tastbl in ('  +
120200170215     C                             %trim(Tipi_Bolla)  + ') ) '              +
120300170216      *  UNION con TITASP in modo da leggere insieme TAS10 e TASP
120400170216      *  l'SQL resta identica a quella scritta sopra per TAS10
120500170216     C                             '  UNION  '                              +
120600170215     C                               ' (SELECT TITASP0F.* , CASE WHEN '     +
120700170215     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
120800170215     C                             ' 5, 3) is null THEN tasvlf ELSE '       +
120900170215     C                             'dec(dec(substr(ar5uni, 10, 5), 5)/1000,'+
121000170215     C                             ' 5, 3) END AS VOL, CASE WHEN '          +
121100170215     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
121200170215     C                             ' 1) is null THEN 0 ELSE '               +
121300170215     C                             'dec(dec(substr(ar5uni, 2, 7), 7)/10, 7,'+
121400170215     C                             ' 1) END AS PES, '
121500170215     c                   if        partip ='T'
121600170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
121700170215     C                             ' tasTFP '
121800170215     c                   elseif    partip ='P'
121900170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
122000170215     C                             ' CASE WHEN  orgfil is null THEN tasTFP '+
122100170215     C                             ' ELSE tasLNP END '
122200170215     c                   elseif    partip ='A'
122300170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
122400170215     C                             ' CASE WHEN  orgfil is null THEN tasTFA '+
122500170215     C                             ' ELSE tasLNA END '
122600170215     c                   end
122700170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
122800170215     C                             ' as LINEA    FROM '                     +
122900170215     C                             ' TITASP0F left outer JOIN FIAR5P0F '    +
123000170215     C                             ' ON ar5trd=''FAT'' and ar5aas = tasaas '+
123100170215     C                             ' and ar5lnp = taslnp and ar5nrs = '     +
123200170215     C                             ' tasnrs and ar5nsp = tasnsp '           +
123300170215     C                             ' left outer JOIN  LINEEITA  on '
123400170215     c                   if        partip ='T'
123500170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
123600170215     C                             ' tasTFP = orgfil '
123700170215     c                   elseif    partip ='P'
123800170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
123900170215     C                             ' tasLNP = orgfil '
124000170215     c                   elseif    partip ='A'
124100170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
124200170215     C                             ' tasLNA = orgfil '
124300170215     c                   end
124400170215     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
124500170215     C                             ' WHERE tasaas = ' + %editc(parann:'X')  +
124600170215     C                             ' and substr(digits(tasmgs), 1, 2) = '   +
124700170215     C                             %editc(parmes:'X') + ' and tastbl in ('  +
124800170215     C                             %trim(Tipi_Bolla)  + ') ) '              +
124900170224     C                             '  )  '                                  +
125000170216      ** --------------
125100170224      **  DA QUI l'Istruzione di LETTURA
125200170224      **  collega file delle SOCIETA in JOIN con file SPEDIZIONI
125300170215      ** --------------
125400170224     C                             'SELECT * FROM ('
125500170224      ** CHIODO per TERMINAL
125600170224     c                   if        partip ='T'
125700170224     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
125800170224     C                             'SELECT case when ID_SOCIETA =''257'' '  +
125900170224     C                             ' and ID_FILIALE = 172 then ''201'' '    +
126000170224     C                             ' else ID_SOCIETA END as SOCCLI,'
126100170224     c                   else
126200170224      ** REGOLARE x le altre estrazioni
126300170224     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
126400170224     C                             ' SELECT ID_SOCIETA as SOCCLI,'
126500170224     c                   end
126600170224     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
126700170224     C                             ' SPEDIZIONI.* FROM '                    +
126800170104     C                             'SPEDIZIONI join AZSOF00F on LINEA '     +
126900170104     C                             '= ID_FILIALE WHERE '                    +
127000170104     C                             ' VALIDITA_DATA_INIZIO <= varchar(DATE(' +
127100170104     C                             'TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas), +
127200170104     C                              DIGITS(tasmgs)), ''YYYYMMDD'') ) , iso) +
127300170104     C                             and VALIDITA_DATA_FINE >= varchar(DATE(' +
127400170104     C                             'TIMESTAMP_FORMAT(CONCAT(DIGITS(tasaas), +
127500170104     C                             DIGITS(tasmgs)), ''YYYYMMDD'')), iso)    +
127600170104     C                             and id_societa <> ''' + parsoc + ''' '   +
127700170216      *  ORDINAMENTO in funzione del tipo Lancio: LINEA e CAMPO
127800170224     C                             %trim(tipo) + ' ) as FILE ORDER BY '     +
127900170224     C                             'SOCCLI, LINEA, '                        +
128000170224     C                             %trim(campo) + ', tasaas , tasmgs'
128100170215     c                   if        partip ='T'
128200170109     C                   EVAL        WrkSqlCmd = %trim(WrkSqlCmd)           +
128300170109     C                             ', tasLNP '
128400170109     C                   END
128500170216      *
128600161212     C                   ENDSR
128700161212     C*------------------------------------------------------------------------*
128800161213     C* STAFINE- STAMPA FINE PROSPETTO
128900161212     C*------------------------------------------------------------------------*
129000161213     C     STAFINE       BEGSR
129100161219      *--
129200161219     c                   if        stampa_dett ='S'
129300170117     c                             AND parSTP  ='S'
129400161214     C                   WRITE     DEEC28FI
129500161219     c                   end
129600161219      *--
129700161219     c                   if        stampa_riep ='S'
129800170117     c                             AND parSTP  ='S'
129900161214     C                   WRITE     RIEC27FI
130000161219     c                   end
130100161219      *--
130200161212     C                   ENDSR
130300161213     C*------------------------------------------------------------------------*
130400100107     OQSYSPRT   E            TES            2 02
130500100107     O                       STM(1)              66
130600100107     O                       STM(2)             132
130700100107     O                       PARANN        Z     80
130800100107     O                       PARMES        Z     83
130900100107     O                       TEDSIM              99
131000100107     O                       DATA          8    111
131100100107     O                       ORA                121 '0 :  :  '
131200100107     O                       PAGE          Z    132
131300100107     O          E            DET         1
131400100107     O                       STM(5)              66
131500100127     O                       parann        Z     39
131600161221     O                       contaD              42
131700161214     O                       TOTVOLGEN     O     75
131800161212     o                                           80 '(MC)'
131900161214     O                  44   TOTPESGEN     O    100
132000161213     o                  44                      105 '(KG)'
132100161214     O                  44   TOTNCLGEN     O    125
132200161213     o                  44                      130 '(NR)'
132300100107     O          E            DET         1
132400100107     O                       STM(6)              66
132500100127     O                       parann        Z     39
132600161221     O                       contaR              42
132700161214     O                       TOTVOLGEN     O     75
132800161212     o                                           80 '(MC)'
132900161214     O                  44   TOTPESGEN     O    100
133000161213     o                  44                      105 '(KG)'
133100161214     O                  44   TOTNCLGEN     O    125
133200161213     o                  44                      130 '(NR)'
133300000000     O*------------------------------------------------------------------------*
133400990420**   STM - DECODIFICHE RIGHE DI STAMPA
133500161128TREC28R                       SINTESI FATTURAZIONE             MC 1
133600100107ANNO MESE 9999/99     XXXXXXXXXXX  99/99/9999  99:99:99   PAG.99992
133700161216ALLEGATO                                                          3
133800161216ALLEGATO DISTRIBUZIONE                                            4
133900161216DETTAGLIO                                                         5
134000161216RIEPILOGO                                                         6
134100161216TRAINI                                                            7
134200161216PARTENZA                                                          8
134300161216ARRIVO                                                            9
134400990420SIMULAZIONE                                                       10
134500161220TERMINAL PARTENZA                                                 11
134600170117LINEA/HUB PARTENZA                                                12
134700170117LINEA/HUB ARRIVO                                                  13
134800161220                                                                  14
134900161220                                                                  15
