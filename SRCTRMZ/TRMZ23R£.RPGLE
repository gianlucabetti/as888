000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - IMMISSIONE PICCOLE MANUTENZIONI -              *
000300011130     FTRmz23D   CF   E             WORKSTN
000400000000     F                                     SFILE(GA23DSF1:NRR)
000500000000     F                                     SFILE(GA23DSF2:NR1)
000600011130     FmzAME01L  IF   E           K DISK
000700011130     F                                     RENAME(mzAME000:mzAME1)
000701070613     FmzAME04L  IF   E           K DISK
000702070613     F                                     RENAME(mzAME000:mzAME4)
000800011130     FmzAME02L  IF   E           K DISK
000900011210     FmzIMP01L  UF A E           K DISK
001000011210     FmzTIM01L  IF   E           K DISK
001100020703     Fmzimp03L  IF   E           K DISK
001200020703     F                                     RENAME(mzimp000:mzimp3)
001300000000     FTABEL00F  IF   E           K DISK
001400011220     D dsxa          e ds
001401070613     D dsxn          e ds
001500011220     D DTAiso          S               D   INZ
001600020703     D prkm            S              1
001700020703     D anno            S              4  0
001800011130     D mese            S              2  0
001900011130     D*--------------------------------------------------------------*
002000021022     D obq             S              1    DIM(31)                              * CHILOMETRI
002001021022     D qt              S              9  2 DIM(31)                              * CHILOMETRI
002100020703     D KM              S              7  0 DIM(31)                              * CHILOMETRI
002200000000     D PR              S              1    DIM(31)                              *
002300000000     D*--------------------------------------------------------------*
002400000000     D GGDS            DS
002500000000     D  GG                     1     31
002600000000     D                                     DIM(31)                              * GIORNI
002700970207     D  SF101                  1      1
002800970207     D  SF102                  2      2
002900970207     D  SF103                  3      3
003000000000     D  SF104                  4      4
003100000000     D  SF105                  5      5
003200000000     D  SF106                  6      6
003300000000     D  SF107                  7      7
003400000000     D  SF108                  8      8
003500000000     D  SF109                  9      9
003600000000     D  SF110                 10     10
003700000000     D  SF111                 11     11
003800000000     D  SF112                 12     12
003900000000     D  SF113                 13     13
004000000000     D  SF114                 14     14
004100000000     D  SF115                 15     15
004200000000     D  SF116                 16     16
004300000000     D  SF117                 17     17
004400000000     D  SF118                 18     18
004500000000     D  SF119                 19     19
004600000000     D  SF120                 20     20
004700000000     D  SF121                 21     21
004800000000     D  SF122                 22     22
004900000000     D  SF123                 23     23
005000000000     D  SF124                 24     24
005100000000     D  SF125                 25     25
005200000000     D  SF126                 26     26
005300000000     D  SF127                 27     27
005400000000     D  SF128                 28     28
005500000000     D  SF129                 29     29
005600000000     D  SF130                 30     30
005700000000     D  SF131                 31     31
005800000000     D KPJBA         E DS
005900000000     D  LIBSYS                92    100
006000000000     D  MSGERR               453    502
007000020703     D ut§dse0f      E DS
007100020703     D dsxm          E DS
007200020704     D salkm           s                   like(impkm)
007300000000      * INDICATORI
007400000000      *
007500000000      * 05 CHAIN SFL1
007600000000      * 06 CHAIN SFL2
007700011210      * 20 POSIZIONAMENTO SU mzIMP000
007800011210      * 21 READE mzIMP000
007900000000      * 22 CHAIN TABEL
008000011210      * 23 CHAIN mzIMP
008100011210      * 24 CHAIN mzAME
008200000000      * N78 SFLNXTCHG
008300000000      * 50 N.RIFERIMENTO ERRATO
008400000000      * 51 DATA ERRATA
008500000000      * 52 GIORNO ERRATO
008600020703      * 53 KM errati
008700020703      * 54 KM errati
008800000000      * 70 PULIZIA SUBF1
008900000000      * 75 PULIZIA SUBF2
009000020703      * 77 QTA obbligatora
009100000000      * 78 PROTEZIONE CAMPI SUBF2
009200020703      * 79 KM obbligatori
009300000000     C*----------------------------------------------------*
009400011130     C                   IF        NOT *INLR
009500011130     C                   MOVEL     *BLANKS       GG
009600011130     C                   Z-ADD     0             KM
009700020703     C                   Z-ADD     0             qt
009800011130     C                   MOVEL     *BLANKS       PR
009900011130     c                   exsr      clr01
010000011130     C                   DO        *HIVAL
010100011130     c                   EXSR      CLR00
010200011130     c                   if        *in99
010300011130     c                   setoff                                       99
010400011130     c                   write     ga23d001
010500011130     c                   seton                                        99
010600011130     c                   end
010700000000     C                   EXFMT     GA23D001
010800011130     C* FINE LAVORO
010900011130     C   KC              LEAVE
011000011130     C* RICERCA
011100011130     C   KD              EXSR      RIC01
011101050706     C* anagrafica automezzi
011103050706     c   kg              call      'TRMZ02R'
011104050706     c                   parm                    kpjba
011105050720     C   kg              ITER
011200011130     C* SCELTA N.RIF. O
011300011130     C                   EXSR      CTR01
011400011130     C   99              ITER
011500020703     c* recupero ultimo chilometraggio del mese precedente
011600020703     c                   exsr      srkm
011700000000     C* PULIZIA SUBFILE
011800000000     C                   EXSR      PULIZ
011900000000     C* CARICAMENTO SUBF1
012000000000     C                   EXSR      CARSF1
012100000000     C* CARICAMENTO DEL SUBF CONTROL
012200020704     C                   EXSR      CARCON
012300011130     C     SVNRR         COMP      0                                  7171
012400011130     C                   DO        *HIVAL
012500011203     C                   WRITE     GA23D002
012600011130     c                   if        *in99
012700011130     c                   setoff                                       99
012800011130     c                   write     ga23dSC1
012900011130     c                   seton                                        99
013000011130     c                   end
013100000000     C                   EXFMT     GA23DSC1
013200011130     C* FINE  LAVORO
013300011130     C   KC              LEAVE
013400011130     C* ritorno
013500011130     c                   if        *inkl
013600011130     c                   exsr      clr01
013700011130     C                   LEAVE
013800011130     c                   end
013900000000     C* CONTROLLI SUBF1
014000011130     C     SVNRR         IFne      0
014100000000     C                   EXSR      CONSF1
014200011130     C   99              iter
014300000000     C* CARICAMENTO SUBF2
014400000000     C                   EXSR      PULIZ1
014500000000     C                   EXSR      CARSF2
014600011130     C                   WRITE     GA23D003
014700011203     C     SVNRR         COMP      0                                  7272
014800011130     c                   do        *hival
014900011130     c                   if        *in99
015000011130     c                   setoff                                       99
015100011130     c                   write     ga23dSC2
015200011130     c                   seton                                        99
015300011130     c                   end
015400000000     C                   EXFMT     GA23DSC2
015500011130     C* FINE  LAVORO
015600011130     C   KC              LEAVE
015700011130     C* ritorno
015800011130     C   kl              LEAVE
015900000000     C* CONTROLLI SUBF2
016000011130     C     SVNRR         IFNE      0
016100000000     C                   EXSR      CONSF2
016200011130     C   99              iter
016300011130     C* ritorno
016400011130     C   kf              LEAVE
016500011130     c                   end
016600011130     C                   enddo
016700011203     C                   END
016800011130     c   KC              leave
016900011210     C* AGGIORNAMENTO mzIMP
017000011130     c                   if        *inkf
017100011130     C                   EXSR      AGGAIM
017200011130     C                   EXSR      clr01
017300011130     c                   leave
017400011130     c                   end
017500011130     C                   enddo
017600011130     c   kc              leave
017700011130     C                   enddo
017800011130     C                   endif
017900000000     C                   SETON                                        LR
018000011130     c**********************************************************************
018100011130     c* pulisce
018200011130     c**********************************************************************
018300011130     C     clr00         BEGSR
018400011130     C                   Z-ADD     0             §RIGA
018500011130     C                   Z-ADD     0             §COLON
018600011130     C                   CLEAR                   §FLD
018700011130     C                   ENDSR
018800011130     C******************************************************
018900011130     c* pulisce 1° formato
019000011130     c**********************************************************************
019100011130     C     clr01         BEGSR
019200011130     C                   Z-ADD     0             VNRF
019300011130     C                   Z-ADD     0             VMM
019400040907     C                   move      *date         VAA
019401070613     c                   clear                   vcau
019402070613     c                   clear                   dcau
019500011130     C                   ENDSR
019600011130     C******************************************************
019700011130     C* ricerca
019800011130     C******************************************************
019900011130     C     ric01         BEGSR
020000020730     c* targa
020100011130     c                   select
020200011221     C                   when      §fld = 'VTGN' or §fld = 'VTGA'
020300011221     C                   CALL      'TRMZ02R4'    RICTAR
020401070613     C                   WHEN      §FLD = 'VCAU'
020402070613     c                   if        naut > 1
020403070613     c                   move      ' '           ordina
020404070613     c                   else
020405070613     c                   move      '1'           ordina
020406070613     c                   movel     *blanks       vcau
020407070613     c                   end
020408070613     c                   z-add     0             vnrf
020409070613     C                   CALL      'TRMZ41R'
020410070613     c                   parm                    ordina            1
020411070613     c                   parm                    vcau
020412070613     c                   parm                    vnrf
020413070613     c                   parm                    cmd               1
020414070613     c                   if        cmd = 'L'
020415070613     c                   clear                   vnrf
020416070613     C                   END
020418070613     C                   ENDsl
020500011130     C* POSIZIONAMENTO CURSORE
020600011130     C                   Z-ADD     §RIGA         §RIG
020700011130     C                   Z-ADD     §COLON        §COL
020800011130     C                   ENDSR
020900011130     C******************************************************
021000011130     C* CONTROLLI PRIMO VIDEO
021100011130     C******************************************************
021200011130     C     CTR01         BEGSR
021300011130     C                   SETOFF                                       99
021400011130     C                   DO
021401070613     C* LETTURA X AUTISTA
021402070613     c                   z-add     0             naut
021403070613     C     VCAU          IFNE      *BLANKS
021404070613     C     VNRF          andeq     0
021405070613     C     vcau          setll     mzAME4
021406070613     c                   do        *hival
021407070613     c     vcau          reade     mzame04l                               21
021408070613     c   21              leave
021409070613     c                   add       1             naut              2 0
021410070613     c                   if        naut > 1
021411070613     c                   eval      §fld = 'VCAU'
021412070613     c                   exsr      ric01
021413070613     c* non è stata fatta alcuna scelta
021414070613     c                   if        cmd = 'L'
021415070613     c                   seton                                        99
021416070613     c                   end
021417070613     c                   leave
021418070613     c                   end
021419070613     c                   enddo
021420070613     c* non trovato automezzo con l'autista scelto
021421070613     c                   if        *in21 and naut = 0
021422070613     C                   SETON                                        4899
021423070613     C                   leave
021424070613     C                   END
021425070613     c                   if        naut <> 0
021426070613     c                   if        not *in99
021427070613     c                   eval      vnrf = amenrf
021428070613     c                   setoff                                       21
021429070613     c                   else
021430070613     c                   leave
021431070613     c                   end
021432070613     c                   end
021433070613     C                   END
021500011130     C* N.TARGA
021600011130     C     VNRF          IFEQ      0
021700011130     C     VTGA          ANDEQ     *BLANKS
021800011130     C                   SETON                                        5099
021900011130     C                   LEAVE
022000011130     C                   END
022100011130     C     VNRF          IFNE      0
022200011130     C     VTGA          ANDNE     *BLANKS
022300011130     C                   MOVE      *BLANKS       VTGA
022400011130     C                   MOVE      *BLANKS       VTGN
022500011130     C                   END
022600011130     C* CONTROLLO TARGA
022700011130     C     VTGA          IFNE      *BLANKS
022800011130     C     VTGN          ANDNE     *BLANKS
022900011130     C     KTAR          CHAIN     MZAME000                           49
023000011130     C                   IF        NOT *IN49
023100011130     C                   Z-ADD     AMENRF        VNRF
023101070613     c                   eval      vcau = amecau
023200011130     C                   ELSE
023300011130     C                   SETON                                        99
023400011130     C                   LEAVE
023500011130     C                   END
023600011130     C                   END
023700011130     C     VNRF          CHAIN     MZAME1                             50
023800011130     C                   IF        *IN50
023900011130     C                   SETON                                        99
024000011130     C                   leave
024001070613     c                   else
024002070613     c                   eval      vcau = amecau
024100011130     C                   END
024200011130     C* CONTROLLO DATA
024300011130     C     VMM           IFEQ      0
024400011130     C     Vaa           orEQ      0
024500011130     C                   SETON                                        5199
024600011130     C                   leave
024700011130     C                   ELSE
024800011130     C                   MOVE      VMM           MA                6 0
024900011130     C                   MOVEL     VAA           MA
025000011130     C                   MOVEl     MA            DATimp            8 0
025100011130     C                   MOVE      01            datimp
025200011130     C     *iso          test(d)                 datimp                 51
025300011130     c                   if        *in51
025400011130     C                   SETON                                        99
025500011130     C                   leave
025600011130     c                   else
025700011130     c                   move      datimp        dtaiso
025800011130     c                   adddur    1:*m          dtaiso
025900011130     c                   subdur    1:*d          dtaiso
026000011130     c                   extrct    dtaiso:*d     salgg             2 0
026100011130     c                   move      dtaiso        datimp
026200011130     C                   END
026300011130     C                   END
026400020730     C* controllo se RIMORCHIO O SEMIRIMORCHIO x errore richiesta KM
026500011130     C                   MOVEL     'XA'          COD
026600011130     C                   MOVE      *BLANKS       KEY
026700011130     C                   MOVEL     AMECAT        KEY
026800011130     C     KTAB1         CHAIN     TABEL
026900011130     c                   if        %found(tabel00f)
027000011220     C                   MOVEl     TBLUNI        dsxa
027100011130     c                   else
027200011220     C                   MOVEl     *BLANKS       dsxa
027300011130     c                   end
027301070613     c* decodifica autista
027303070613     c                   movel     'XN'          cod
027304070613     c                   movel(P)  VCAU          key
027305070613     C     KTAB1         CHAIN     TABEL
027306070613     c                   if        %found(tabel00f)
027307070613     C                   MOVEl     TBLUNI        dsxn
027308070613     c                   eval      dcau = §XNDES
027309070613     c                   else
027310070613     C                   MOVEl     *BLANKS       dsxn
027311070613     c                   clear                   dcau
027312070613     c                   end
027400011130     C                   ENDDO
027500011130     C*
027600011130     C                   ENDSR
027700011130     C******************************************************
027800011130     C** PULIZIA DEL SUBFILE
027900011130     C******************************************************
028000011130     C     PULIZ         BEGSR
028100011130     C                   SETON                                        70
028200011130     C                   WRITE     GA23DSC1
028300011130     C                   SETOFF                                       70
028400011130     C                   ENDSR
028500011130     C******************************************************
028600011130     C** PULIZIA DEL SUBFILE
028700011130     C******************************************************
028800011130     C     PULIZ1        BEGSR
028900011130     C                   SETON                                        75
029000011130     C                   WRITE     GA23DSC2
029100011130     C                   SETOFF                                       75
029200011130     C                   ENDSR
029300000000     C******************************************************
029400011130     C** CARICAMENTO SUBF1
029500011130     C******************************************************
029600000000     C     CARSF1        BEGSR
029700000000     C                   Z-ADD     0             NRR               4 0
029800011203     C                   clear                   KM
029900020703     C                   clear                   qt
030100000000     C* LETTURA TABELLA XM
030200000000     C                   MOVEL     'XM'          COD
030300000000     C     KTAB          SETLL     TABEL
030400011130     c                   do        *hival
030500000000     C     KTAB          READE     TABEL                                  22
030600011130     C   22              leave
030700970205     C                   MOVEL     TBLUNI        WESCLU            2
030800910211     C     TBLFLG        IFEQ      *BLANKS
030900970205     C     WESCLU        ANDNE     'NN'
031000000000     C                   MOVEL     TBLKEY        SF1CIN
031100020703     C*                  MOVEL     TBLUNI        SF1DIN
031200020703     C                   MOVEL     TBLUNI        dsxm
031300020703     C                   MOVEL     §xmdes        sf1din
031400020703     C                   MOVEL     §xmtip        sf1tip
031500891120     C* CARICO UNITA DI MISURA
031600891120     C                   MOVEL     SF1CIN        TIMINT
031700891120     C                   MOVEL     'P'           TIMTIP
031800891120     C                   MOVEL     AMEUTI        TIMMAN
031900011210     C     KTIM          CHAIN     mzTIM01L                           21
032000020801     c                   if        not *in21
032100020801     C                   MOVEL     TIMUNI        SF1UNI
032200020801     c                   else
032300020801     C                   MOVEL     *blanks       SF1UNI
032400020801     c                   end
032500891120     C                   SETOFF                                       21
032600011210     C* LETTURA DI mzIMP
032700000000     C                   Z-ADD     DATIMP        IMPDAT
032800000000     C                   MOVE      00            IMPDAT
032900011130     C                   clear                   GG
033000011210     C     KGAI          SETLL     mzIMP000
033100011130     c                   do        *hival
033200011210     C     KGAI2         READE     mzIMP000                               21
033300011130     c   21              leave
033400000000     C*CONTROLLO DATA
033500011130     C                   MOVE      IMPDAT        Dtaiso
033600011130     c                   extrct    dtaiso:*y     anno
033700011130     c                   extrct    dtaiso:*m     mese
033800011130     C     Anno          IFNE      VAA
033900011130     C     mese          ORNE      VMM
034000011130     C                   leave
034100000000     C                   END
034200011130     c                   extrct    dtaiso:*d     g1                2 0
034300000000     C*ISOLO IL GIORNO DI IMPDAT
034400000000     C*CARICO I GIORNI E I  KM.
034500000000     C                   MOVE      'X'           GG(G1)
034600020801     C                   IF        SF1UNI = 'KM'
034700000000     C                   Z-ADD     IMPKM         KM(G1)
034800020704     c                   if        sf1tip <> ' '
034900020703     C                   Z-ADD     IMPqta        qt(G1)
035100020704     c                   end
035200020801     c                   end
035300011130     C                   ENDdo
035400011130     C                   ADD       1             NRR
035500011130     C                   WRITE     GA23DSF1
035600000000     C                   END
035700011130     C                   ENDdo
035800011130     C                   Z-ADD     NRR           SVNRR             4 0
035900000000     C                   ENDSR
036000000000     C******************************************************
036100000000     C** CARICAMENTO CONTROL
036200020730     C** CON £2 CON ULTIMO
036300000000     C** GIORNO DEL MESE
036400011130     C******************************************************
036500020704     C     CARCON        BEGSR
036600000000     C*CONSUMO OLIO
036700020704     C                   MOVEL     '£2'          INT
036800020704     C     KGAI3         CHAIN     mzIMP000                           23
036900020704     C   23              Z-ADD     0             SC1COL
037000020704     C  N23              Z-ADD     IMPQTA        SC1COL
037100020704     C     VNRF          CHAIN     mzAME1                             24
037200020704     C                   ENDSR
037300000000     C******************************************************
037400011130     C** CONTROLLI SUBF1
037500011130     C******************************************************
037600000000     C     CONSF1        BEGSR
037700011203     C                   CLEAR                   PR
037701021022     C                   clear                   obq
037800020730     c                   setoff                                       995257
037900020730     c                   setoff                                       5859
038000011130     C                   DO        *HIVAL        A                 4 0
038100000000     C     A             CHAIN     GA23DSF1                           05
038200011130     C   05              leave
038300020730     c* controllo che ci sia selezionato almeno un gg (se no rimorchio)
038400020730     c                   if        §xarsm <> 'S'
038500020705     c                             and ggds = *blanks
038600020730     c                   select
038700020730     c                   when      sf1cin = '£1'
038800020730     C                   SETON                                        58
038900020730     c                   when      sf1cin = 'CB'
039000020730     C                   SETON                                        59
039100020730     C                   ENDsl
039200020705     C                   ENDif
039300020703     c* controllo ultimo gg del mese
039400020730     C                   DO        31            B                 2 0
039500000000     C     GG(B)         IFNE      *BLANKS
039600011130     C     B             IFGT      salgg
039700011130     C                   SETON                                        5299
039800011130     C   52              leave
039900011130     C                   ENDif
040000910214     C     SF1UNI        IFEQ      'KM'
040100000000     C                   MOVEL     GG(B)         PR(B)
040200011130     C                   ENDif
040201021022     c                   if        sf1tip <> ' '
040202021022     C                   MOVE      sf1tip        obq(B)
040300011130     C                   ENDif
040301021022     C                   ENDif
040400011130     C                   ENDdo
040500011203     C                   UPDATE    GA23DSF1
040600020730     c                   if        *in52
040700011130     c                   leave
040800011130     C                   ENDif
040900020730     C                   ENDdo
041000020730     c* se non inserito almeno un gg di carburante errore
041100020730     c                   if        *in58 and *in59
041200020730     C                   SETON                                        5799
041300020730     C                   ENDif
041400011130     C*
041500011130     C                   ENDSR
041600000000     C******************************************************
041700011130     C** CARICAMENTO SUBF2
041800011130     C******************************************************
041900000000     C     CARSF2        BEGSR
042000000000     C                   Z-ADD     0             NR1               4 0
042100000000     C                   DO        31            A
042200000000     C                   Z-ADD     A             SF2GG
042300000000     C     PR(A)         IFEQ      *BLANKS
042400000000     C                   Z-ADD     0             SF2KM1
042500020704     C                   Z-ADD     0             SF2cca
042600021031     C                   SETON                                        7880
042700000000     C                   ELSE
042800021031     C                   SETOFF                                       7880
042900000000     C                   Z-ADD     KM(A)         SF2KM1
042901021022     c                   if        obq(a) = ' '
042902021031     c                   seton                                        80
042903021022     c                   z-add     0             sf2cca
042904021022     c                   else
042905021031     c                   setoff                                       80
043000020703     C                   Z-ADD     qt(A)         SF2cca
043001021022     c                   end
043002021022     C                   move      obq(a)        SF2tip
043100020703     C                   END
043200000000     C                   ADD       1             NR1
043300000000     C                   WRITE     GA23DSF2
043400000000     C                   END
043500011130     C                   Z-ADD     NR1           SVNRR             4 0
043600000000     C                   ENDSR
043700000000     C******************************************************
043800011130     C** CONTROLLI SUBF2
043900011130     C******************************************************
044000000000     C     CONSF2        BEGSR
044100020703     c*
044200011130     C                   CLEAR                   KM1
044300020703     C                   CLEAR                   prkm
044400021031     C                   SETOFF                                       809978
044500011130     C                   DO        *HIVAL        A                 4 0
044600000000     C                   READC     GA23DSF2                               06
044700011130     C   06              leave
044800020704     c                   setoff                                       55
044900020703     c* controllo km
045000000000     C     SF2KM1        IFEQ      0
045100020704     C                   SETON                                        559979
045200000000     C                   ELSE
045300020704     C                   SETOFF                                           79
045400020705     C                   END
045500020703     c* controllo che ci sia congruenza nei km immessi
045600000000     C     SF2KM1        IFLT      KM1
045700020704     C                   SETON                                        559954
045800000000     C                   END
045900020703     c* controllo che ci sia congruenza nei km immessi nei mesi precedenti
046000020703     C                   IF        prkm = *blanks
046100020703     c                   eval      prkm = '1'
046200020703     C     SF2KM1        IFLT      KMp
046300020704     C                   SETON                                        559953
046400020703     C                   END
046500020703     C                   END
046600020703     c* controllo qta
046700021022     c                   if        sf2tip = '1'
046701021031     c                   setoff                                       80
046800020703     C     SF2cca        IFEQ      0
046900020703     C                   SETON                                          9977
047000020703     C                   ELSE
047100020703     C                   SETOFF                                           77
047200020703     C                   END
047201021031     C                   ELSE
047202021031     c                   seton                                        80
047300020703     C                   END
047400000000     C                   UPDATE    GA23DSF2
047500000000     C                   Z-ADD     SF2KM1        KM1               7 0
047600000000     C                   Z-ADD     SF2KM1        KM(NR1)
047700020703     C                   Z-ADD     SF2cca        qt(NR1)
047800000000     C  N99              END
047900020704     c* controllo che ci sia congruenza nei km immessi nei mesi successivi
048000020704     C     SF2KM1        IFgT      KMd
048100020704     c     kmd           andgt     0
048200021031     c     nr1           chain     ga23dsf2                           81
048300021031     c                   if        not *in81
048301021031     c                   if        sf2tip = '1'
048302021031     c                   setoff                                       80
048308021031     C                   ELSE
048309021031     c                   seton                                        80
048310021031     C                   END
048400020704     C                   SETON                                        559956
048500020704     c                   update    ga23dsf2
048600020704     C                   END
048700020704     C                   END
048800011130     C                   ENDSR
048900000000     C******************************************************
049000020704     C** reperisce km del mese precedente e del mese successivo
049100011130     C******************************************************
049200020703     C     srkm          BEGSR
049300020703     c*
049400020703     C                   CLEAR                   KMp               7 0
049500020703     C                   Z-ADD     DATIMP        IMPDAT
049600020703     C                   MOVE      00            IMPDAT
049700020703     c     kimp          setgt     mzimp03l
049800020703     c                   do        *hival
049900020703     c     vnrf          readpe    mzimp03l
050000020703     c                   if        %eof(mzimp03l)
050100020703     c                   leave
050200020703     c                   end
050300020703     c                   z-add     impkm         kmp
050400020703     c                   if        kmp <>0
050500020703     c                   leave
050600020703     c                   end
050700020703     c                   enddo
050800020704     c*
050900020704     C                   CLEAR                   KMd               7 0
051000020704     C                   Z-ADD     DATIMP        IMPDAT
051100020704     C                   MOVE      31            IMPDAT
051200020704     c     kimp          setgt     mzimp03l
051300020704     c                   do        *hival
051400020704     c     vnrf          reade     mzimp03l
051500020704     c                   if        %eof(mzimp03l)
051600020704     c                   leave
051700020704     c                   end
051800020704     c                   z-add     impkm         kmd
051900020704     c                   if        kmd <>0
052000020704     c                   leave
052100020704     c                   end
052200020704     c                   enddo
052300020703     c*
052400020703     C                   ENDSR
052500020703     C******************************************************
052600020703     C** AGGIORNAMENTO mzIMP
052700020703     C******************************************************
052800020703     C     AGGAIM        BEGSR
052900020704     c                   z-add     0             salkm
053000011130     C                   DO        *HIVAL        A
053100000000     C     A             CHAIN     GA23DSF1                           05
053200011130     C   05              leave
053300000000     C                   DO        31            B
053400000000     C                   Z-ADD     DATIMP        IMPDAT
053500000000     C                   MOVE      B             IMPDAT
053600011210     C     KGAI          DELETE    mzIMP000                           08
053700000000     C     GG(B)         IFNE      *BLANKS
053800000000     C                   Z-ADD     VNRF          IMPNRF
053900000000     C                   MOVEL     SF1CIN        IMPINT
054000020703     C                   Z-ADD     KM(B)         IMPKM
054100020704     c                   if        impkm <> 0 and impkm > salkm
054200020704     c                   z-add     impkm         salkm
054300020704     c                   end
054400020703     c                   if        sf1tip = ' '
054500000000     C                   Z-ADD     0             IMPQTA
054600020703     c                   else
054700020703     C                   Z-ADD     qt(b)         IMPQTA
054800020703     c                   end
054900011210     C                   WRITE     mzIMP000
055000011130     C                   ENDif
055100011130     C                   ENDdo
055200011130     C                   ENDdo
055300000000     C** CONSUMO OLIO
055400020704     C                   MOVEL     '£2'          INT
055500020704     C     KGAI3         CHAIN     mzIMP000                           08
055600020704     C                   Z-ADD     VNRF          IMPNRF
055700020704     C                   MOVEL     INT           IMPINT
055800020704     C                   Z-ADD     DATIMP        IMPDAT
055900020704     C                   Z-ADD     salkm         IMPKM
056000020704     C                   Z-ADD     SC1COL        IMPQTA
056100020704     C   08              WRITE     mzIMP000
056200020704     C  N08              UPDATE    mzIMP000
056300011130     C                   ENDSR
056400011130     C*---------------------------------------------------------------*
056500011130     c     *inzsr        begsr
056600011130     C*---------------------------------------------------------------*
056700011130     C     *ENTRY        PLIST
056800011130     C                   PARM                    KPJBA
056900011130     C                   Z-ADD     1             CODUT
057000020703     C                   CALL      'X§PARUT'
057100020703     C*                  PARM                    UTEDtE
057200020703     C                   PARM                    UT§DSE0F
057300011130     C                   IF        CODUT = -1
057400011130     C                   SETON                                        LR
057500011130     C                   END
057600011130     C                   MOVEL     RAGUT         RSUT             20
057700011130     C                   MOVEL     RAGUT         RSUT2            25
057800011130     C*----------------------------------------------------*
057900011130     C* POSIZIONAMENTO TABELLE                             *
058000011130     C     KTAB          KLIST                                                  *
058100011130     C                   KFLD                    CODUT                          *
058200011130     C                   KFLD                    COD               2            *
058300011130     C     KTAB1         KLIST                                                  *
058400011130     C                   KFLD                    CODUT                          *
058500011130     C                   KFLD                    COD               2            *
058600011130     C                   KFLD                    KEY               8            *
058700011210     C* POSIZIONAMENTO mzIMP000                            *
058800011130     C     KGAI          KLIST                                                  *
058900011130     C                   KFLD                    VNRF                           *
059000011130     C                   KFLD                    SF1CIN                         *
059100011130     C                   KFLD                    IMPDAT                         *
059200011210     C* LETTURA DI     mzIMP000                            *
059300011130     C     KGAI2         KLIST                                                  *
059400011130     C                   KFLD                    VNRF                           *
059500011130     C                   KFLD                    SF1CIN                         *
059600011130     C* CARICAMENTO CONS.CARB.CONS.OLIO                    *
059700011130     C     KGAI3         KLIST                                                  *
059800011130     C                   KFLD                    VNRF                           *
059900011130     C                   KFLD                    INT               2            *
060000011130     C                   KFLD                    DATIMP            8 0          *
060100020703     C     Kimp          KLIST                                                  *
060200020703     C                   KFLD                    VNRF                           *
060300020703     C                   KFLD                    impdat                         *
060400011210     C* LETTURA DI     mzTIM000                            *
060500011130     C     KTIM          KLIST                                                  *
060600011130     C                   KFLD                    TIMTIP                         *
060700011130     C                   KFLD                    TIMINT                         *
060800011130     C                   KFLD                    TIMMAN                         *
060900011130     C*
061000011130     C* CHAIN X N.DI TARGA                                 *
061100011130     C     KTAR          KLIST                                                  *
061200011130     C                   KFLD                    VTGA                           *
061300011130     C                   KFLD                    VTGN                           *
061400011130     C*
061500011130     C* RICERCA TARGA X CITTA'
061600011130     C     RICTAR        PLIST
061700011130     C                   PARM                    CODUT
061800011130     C                   PARM                    RSUT
061900011130     C                   PARM                    VTGA
062000011130     C                   PARM                    VTGN
062100011130     C*
062200011130     c                   endsr
