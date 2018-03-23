000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - IMMISSIONE PICCOLE MANUTENZIONI -              *
000300011130     FTRmz23D   CF   E             WORKSTN
000400000000     F                                     SFILE(GA23DSF1:NRR)
000500000000     F                                     SFILE(GA23DSF2:NR1)
000600011130     FmzAME01L  IF   E           K DISK
000700011130     F                                     RENAME(mzAME000:mzAME1)
000800070613     FmzAME04L  IF   E           K DISK
000900070613     F                                     RENAME(mzAME000:mzAME4)
001000011130     FmzAME02L  IF   E           K DISK
001100011210     FmzIMP01L  UF A E           K DISK
001200011210     FmzTIM01L  IF   E           K DISK
001300020703     Fmzimp03L  IF   E           K DISK
001400020703     F                                     RENAME(mzimp000:mzimp3)
001500000000     FTABEL00F  IF   E           K DISK
001600160518     FTntbe01l  IF   E           K DISK
001700011220     D dsxa          e ds
001800070613     D dsxn          e ds
001900011220     D DTAiso          S               D   INZ
002000020703     D prkm            S              1
002100020703     D anno            S              4  0
002200011130     D mese            S              2  0
002300011130     D*--------------------------------------------------------------*
002400021022     D obq             S              1    DIM(31)                              * CHILOMETRI
002500021022     D qt              S              9  2 DIM(31)                              * CHILOMETRI
002600020703     D KM              S              7  0 DIM(31)                              * CHILOMETRI
002700000000     D PR              S              1    DIM(31)                              *
002800000000     D*--------------------------------------------------------------*
002900000000     D GGDS            DS
003000000000     D  GG                     1     31
003100000000     D                                     DIM(31)                              * GIORNI
003200970207     D  SF101                  1      1
003300970207     D  SF102                  2      2
003400970207     D  SF103                  3      3
003500000000     D  SF104                  4      4
003600000000     D  SF105                  5      5
003700000000     D  SF106                  6      6
003800000000     D  SF107                  7      7
003900000000     D  SF108                  8      8
004000000000     D  SF109                  9      9
004100000000     D  SF110                 10     10
004200000000     D  SF111                 11     11
004300000000     D  SF112                 12     12
004400000000     D  SF113                 13     13
004500000000     D  SF114                 14     14
004600000000     D  SF115                 15     15
004700000000     D  SF116                 16     16
004800000000     D  SF117                 17     17
004900000000     D  SF118                 18     18
005000000000     D  SF119                 19     19
005100000000     D  SF120                 20     20
005200000000     D  SF121                 21     21
005300000000     D  SF122                 22     22
005400000000     D  SF123                 23     23
005500000000     D  SF124                 24     24
005600000000     D  SF125                 25     25
005700000000     D  SF126                 26     26
005800000000     D  SF127                 27     27
005900000000     D  SF128                 28     28
006000000000     D  SF129                 29     29
006100000000     D  SF130                 30     30
006200000000     D  SF131                 31     31
006300000000     D KPJBA         E DS
006400000000     D  LIBSYS                92    100
006500000000     D  MSGERR               453    502
006600020703     D ut§dse0f      E DS
006700020703     D dsxm          E DS
006800160518     D dmau          E DS
006900160518     D dcau          E DS
007000020704     D salkm           s                   like(impkm)
007100000000      * INDICATORI
007200000000      *
007300000000      * 05 CHAIN SFL1
007400000000      * 06 CHAIN SFL2
007500011210      * 20 POSIZIONAMENTO SU mzIMP000
007600011210      * 21 READE mzIMP000
007700000000      * 22 CHAIN TABEL
007800011210      * 23 CHAIN mzIMP
007900011210      * 24 CHAIN mzAME
008000000000      * N78 SFLNXTCHG
008100000000      * 50 N.RIFERIMENTO ERRATO
008200000000      * 51 DATA ERRATA
008300000000      * 52 GIORNO ERRATO
008400020703      * 53 KM errati
008500020703      * 54 KM errati
008600000000      * 70 PULIZIA SUBF1
008700000000      * 75 PULIZIA SUBF2
008800020703      * 77 QTA obbligatora
008900000000      * 78 PROTEZIONE CAMPI SUBF2
009000020703      * 79 KM obbligatori
009100000000     C*----------------------------------------------------*
009200011130     C                   IF        NOT *INLR
009300011130     C                   MOVEL     *BLANKS       GG
009400011130     C                   Z-ADD     0             KM
009500020703     C                   Z-ADD     0             qt
009600011130     C                   MOVEL     *BLANKS       PR
009700011130     c                   exsr      clr01
009800011130     C                   DO        *HIVAL
009900011130     c                   EXSR      CLR00
010000011130     c                   if        *in99
010100011130     c                   setoff                                       99
010200011130     c                   write     ga23d001
010300011130     c                   seton                                        99
010400011130     c                   end
010500000000     C                   EXFMT     GA23D001
010600011130     C* FINE LAVORO
010700011130     C   KC              LEAVE
010800011130     C* RICERCA
010900011130     C   KD              EXSR      RIC01
011000050706     C* anagrafica automezzi
011100050706     c   kg              call      'TRMZ02R'
011200050706     c                   parm                    kpjba
011300050720     C   kg              ITER
011400011130     C* SCELTA N.RIF. O
011500011130     C                   EXSR      CTR01
011600011130     C   99              ITER
011700020703     c* recupero ultimo chilometraggio del mese precedente
011800020703     c                   exsr      srkm
011900000000     C* PULIZIA SUBFILE
012000000000     C                   EXSR      PULIZ
012100000000     C* CARICAMENTO SUBF1
012200000000     C                   EXSR      CARSF1
012300000000     C* CARICAMENTO DEL SUBF CONTROL
012400020704     C                   EXSR      CARCON
012500011130     C     SVNRR         COMP      0                                  7171
012600011130     C                   DO        *HIVAL
012700011203     C                   WRITE     GA23D002
012800011130     c                   if        *in99
012900011130     c                   setoff                                       99
013000011130     c                   write     ga23dSC1
013100011130     c                   seton                                        99
013200011130     c                   end
013300000000     C                   EXFMT     GA23DSC1
013400011130     C* FINE  LAVORO
013500011130     C   KC              LEAVE
013600011130     C* ritorno
013700011130     c                   if        *inkl
013800011130     c                   exsr      clr01
013900011130     C                   LEAVE
014000011130     c                   end
014100000000     C* CONTROLLI SUBF1
014200011130     C     SVNRR         IFne      0
014300000000     C                   EXSR      CONSF1
014400011130     C   99              iter
014500000000     C* CARICAMENTO SUBF2
014600000000     C                   EXSR      PULIZ1
014700000000     C                   EXSR      CARSF2
014800011130     C                   WRITE     GA23D003
014900011203     C     SVNRR         COMP      0                                  7272
015000011130     c                   do        *hival
015100011130     c                   if        *in99
015200011130     c                   setoff                                       99
015300011130     c                   write     ga23dSC2
015400011130     c                   seton                                        99
015500011130     c                   end
015600000000     C                   EXFMT     GA23DSC2
015700011130     C* FINE  LAVORO
015800011130     C   KC              LEAVE
015900011130     C* ritorno
016000011130     C   kl              LEAVE
016100000000     C* CONTROLLI SUBF2
016200011130     C     SVNRR         IFNE      0
016300000000     C                   EXSR      CONSF2
016400011130     C   99              iter
016500011130     C* ritorno
016600011130     C   kf              LEAVE
016700011130     c                   end
016800011130     C                   enddo
016900011203     C                   END
017000011130     c   KC              leave
017100011210     C* AGGIORNAMENTO mzIMP
017200011130     c                   if        *inkf
017300011130     C                   EXSR      AGGAIM
017400011130     C                   EXSR      clr01
017500011130     c                   leave
017600011130     c                   end
017700011130     C                   enddo
017800011130     c   kc              leave
017900011130     C                   enddo
018000011130     C                   endif
018100000000     C                   SETON                                        LR
018200011130     c**********************************************************************
018300011130     c* pulisce
018400011130     c**********************************************************************
018500011130     C     clr00         BEGSR
018600011130     C                   Z-ADD     0             §RIGA
018700011130     C                   Z-ADD     0             §COLON
018800011130     C                   CLEAR                   §FLD
018900011130     C                   ENDSR
019000011130     C******************************************************
019100011130     c* pulisce 1° formato
019200011130     c**********************************************************************
019300011130     C     clr01         BEGSR
019400011130     C                   Z-ADD     0             VNRF
019500011130     C                   Z-ADD     0             VMM
019600040907     C                   move      *date         VAA
019700070613     c                   clear                   vcau
019800160518     c                   clear                   vdcau
019900011130     C                   ENDSR
020000011130     C******************************************************
020100011130     C* ricerca
020200011130     C******************************************************
020300011130     C     ric01         BEGSR
020400020730     c* targa
020500011130     c                   select
020600011221     C                   when      §fld = 'VTGN' or §fld = 'VTGA'
020700011221     C                   CALL      'TRMZ02R4'    RICTAR
020800070613     C                   WHEN      §FLD = 'VCAU'
020900070613     c                   if        naut > 1
021000070613     c                   move      ' '           ordina
021100070613     c                   else
021200070613     c                   move      '1'           ordina
021300070613     c                   movel     *blanks       vcau
021400070613     c                   end
021500070613     c                   z-add     0             vnrf
021600070613     C                   CALL      'TRMZ41R'
021700070613     c                   parm                    ordina            1
021800070613     c                   parm                    vcau
021900070613     c                   parm                    vnrf
022000070613     c                   parm                    cmd               1
022100070613     c                   if        cmd = 'L'
022200070613     c                   clear                   vnrf
022300070613     C                   END
022400070613     C                   ENDsl
022500011130     C* POSIZIONAMENTO CURSORE
022600011130     C                   Z-ADD     §RIGA         §RIG
022700011130     C                   Z-ADD     §COLON        §COL
022800011130     C                   ENDSR
022900011130     C******************************************************
023000011130     C* CONTROLLI PRIMO VIDEO
023100011130     C******************************************************
023200011130     C     CTR01         BEGSR
023300011130     C                   SETOFF                                       99
023400011130     C                   DO
023500070613     C* LETTURA X AUTISTA
023600070613     c                   z-add     0             naut
023700070613     C     VCAU          IFNE      *BLANKS
023800070613     C     VNRF          andeq     0
023900070613     C     vcau          setll     mzAME4
024000070613     c                   do        *hival
024100070613     c     vcau          reade     mzame04l                               21
024200070613     c   21              leave
024300070613     c                   add       1             naut              2 0
024400070613     c                   if        naut > 1
024500070613     c                   eval      §fld = 'VCAU'
024600070613     c                   exsr      ric01
024700070613     c* non è stata fatta alcuna scelta
024800070613     c                   if        cmd = 'L'
024900070613     c                   seton                                        99
025000070613     c                   end
025100070613     c                   leave
025200070613     c                   end
025300070613     c                   enddo
025400070613     c* non trovato automezzo con l'autista scelto
025500070613     c                   if        *in21 and naut = 0
025600070613     C                   SETON                                        4899
025700070613     C                   leave
025800070613     C                   END
025900070613     c                   if        naut <> 0
026000070613     c                   if        not *in99
026100070613     c                   eval      vnrf = amenrf
026200070613     c                   setoff                                       21
026300070613     c                   else
026400070613     c                   leave
026500070613     c                   end
026600070613     c                   end
026700070613     C                   END
026800011130     C* N.TARGA
026900011130     C     VNRF          IFEQ      0
027000011130     C     VTGA          ANDEQ     *BLANKS
027100011130     C                   SETON                                        5099
027200011130     C                   LEAVE
027300011130     C                   END
027400011130     C     VNRF          IFNE      0
027500011130     C     VTGA          ANDNE     *BLANKS
027600011130     C                   MOVE      *BLANKS       VTGA
027700011130     C                   MOVE      *BLANKS       VTGN
027800011130     C                   END
027900011130     C* CONTROLLO TARGA
028000011130     C     VTGA          IFNE      *BLANKS
028100011130     C     VTGN          ANDNE     *BLANKS
028200011130     C     KTAR          CHAIN     MZAME000                           49
028300011130     C                   IF        NOT *IN49
028400011130     C                   Z-ADD     AMENRF        VNRF
028500070613     c                   eval      vcau = amecau
028600011130     C                   ELSE
028700011130     C                   SETON                                        99
028800011130     C                   LEAVE
028900011130     C                   END
029000011130     C                   END
029100011130     C     VNRF          CHAIN     MZAME1                             50
029200011130     C                   IF        *IN50
029300011130     C                   SETON                                        99
029400011130     C                   leave
029500070613     c                   else
029600070613     c                   eval      vcau = amecau
029700011130     C                   END
029800011130     C* CONTROLLO DATA
029900011130     C     VMM           IFEQ      0
030000011130     C     Vaa           orEQ      0
030100011130     C                   SETON                                        5199
030200011130     C                   leave
030300011130     C                   ELSE
030400011130     C                   MOVE      VMM           MA                6 0
030500011130     C                   MOVEL     VAA           MA
030600011130     C                   MOVEl     MA            DATimp            8 0
030700011130     C                   MOVE      01            datimp
030800011130     C     *iso          test(d)                 datimp                 51
030900011130     c                   if        *in51
031000011130     C                   SETON                                        99
031100011130     C                   leave
031200011130     c                   else
031300011130     c                   move      datimp        dtaiso
031400011130     c                   adddur    1:*m          dtaiso
031500011130     c                   subdur    1:*d          dtaiso
031600011130     c                   extrct    dtaiso:*d     salgg             2 0
031700011130     c                   move      dtaiso        datimp
031800011130     C                   END
031900011130     C                   END
032000020730     C* controllo se RIMORCHIO O SEMIRIMORCHIO x errore richiesta KM
032100011130     C                   MOVEL     'XA'          COD
032200011130     C                   MOVE      *BLANKS       KEY
032300011130     C                   MOVEL     AMECAT        KEY
032400011130     C     KTAB1         CHAIN     TABEL
032500011130     c                   if        %found(tabel00f)
032600011220     C                   MOVEl     TBLUNI        dsxa
032700011130     c                   else
032800011220     C                   MOVEl     *BLANKS       dsxa
032900011130     c                   end
033000070613     c* decodifica autista
033100070613     c                   movel     'XN'          cod
033200070613     c                   movel(P)  VCAU          key
033300070613     C     KTAB1         CHAIN     TABEL
033400070613     c                   if        %found(tabel00f)
033500070613     C                   MOVEl     TBLUNI        dsxn
033600160518     c                   eval      vdcau = §XNDES
033700070613     c                   else
033800070613     C                   MOVEl     *BLANKS       dsxn
033900160518     c                   clear                   vdcau
034000070613     c                   end
034100011130     C                   ENDDO
034200011130     C*
034300011130     C                   ENDSR
034400011130     C******************************************************
034500011130     C** PULIZIA DEL SUBFILE
034600011130     C******************************************************
034700011130     C     PULIZ         BEGSR
034800011130     C                   SETON                                        70
034900011130     C                   WRITE     GA23DSC1
035000011130     C                   SETOFF                                       70
035100011130     C                   ENDSR
035200011130     C******************************************************
035300011130     C** PULIZIA DEL SUBFILE
035400011130     C******************************************************
035500011130     C     PULIZ1        BEGSR
035600011130     C                   SETON                                        75
035700011130     C                   WRITE     GA23DSC2
035800011130     C                   SETOFF                                       75
035900011130     C                   ENDSR
036000000000     C******************************************************
036100011130     C** CARICAMENTO SUBF1
036200011130     C******************************************************
036300000000     C     CARSF1        BEGSR
036400000000     C                   Z-ADD     0             NRR               4 0
036500011203     C                   clear                   KM
036600020703     C                   clear                   qt
036700000000     C* LETTURA TABELLA XM
036800000000     C                   MOVEL     'XM'          COD
036900000000     C     KTAB          SETLL     TABEL
037000011130     c                   do        *hival
037100000000     C     KTAB          READE     TABEL                                  22
037200011130     C   22              leave
037300970205     C                   MOVEL     TBLUNI        WESCLU            2
037400910211     C     TBLFLG        IFEQ      *BLANKS
037500970205     C     WESCLU        ANDNE     'NN'
037600000000     C                   MOVEL     TBLKEY        SF1CIN
037700020703     C*                  MOVEL     TBLUNI        SF1DIN
037800020703     C                   MOVEL     TBLUNI        dsxm
037900020703     C                   MOVEL     §xmdes        sf1din
038000020703     C                   MOVEL     §xmtip        sf1tip
038100891120     C* CARICO UNITA DI MISURA
038200891120     C                   MOVEL     SF1CIN        TIMINT
038300891120     C                   MOVEL     'P'           TIMTIP
038400891120     C                   MOVEL     AMEUTI        TIMMAN
038500011210     C     KTIM          CHAIN     mzTIM01L                           21
038600020801     c                   if        not *in21
038700020801     C                   MOVEL     TIMUNI        SF1UNI
038800020801     c                   else
038900020801     C                   MOVEL     *blanks       SF1UNI
039000020801     c                   end
039100891120     C                   SETOFF                                       21
039200011210     C* LETTURA DI mzIMP
039300000000     C                   Z-ADD     DATIMP        IMPDAT
039400000000     C                   MOVE      00            IMPDAT
039500011130     C                   clear                   GG
039600011210     C     KGAI          SETLL     mzIMP000
039700011130     c                   do        *hival
039800011210     C     KGAI2         READE     mzIMP000                               21
039900011130     c   21              leave
040000000000     C*CONTROLLO DATA
040100011130     C                   MOVE      IMPDAT        Dtaiso
040200011130     c                   extrct    dtaiso:*y     anno
040300011130     c                   extrct    dtaiso:*m     mese
040400011130     C     Anno          IFNE      VAA
040500011130     C     mese          ORNE      VMM
040600011130     C                   leave
040700000000     C                   END
040800011130     c                   extrct    dtaiso:*d     g1                2 0
040900000000     C*ISOLO IL GIORNO DI IMPDAT
041000000000     C*CARICO I GIORNI E I  KM.
041100000000     C                   MOVE      'X'           GG(G1)
041200020801     C                   IF        SF1UNI = 'KM'
041300000000     C                   Z-ADD     IMPKM         KM(G1)
041400020704     c                   if        sf1tip <> ' '
041500020703     C                   Z-ADD     IMPqta        qt(G1)
041600020704     c                   end
041700020801     c                   end
041800011130     C                   ENDdo
041900011130     C                   ADD       1             NRR
042000011130     C                   WRITE     GA23DSF1
042100000000     C                   END
042200011130     C                   ENDdo
042300011130     C                   Z-ADD     NRR           SVNRR             4 0
042400000000     C                   ENDSR
042500000000     C******************************************************
042600000000     C** CARICAMENTO CONTROL
042700020730     C** CON £2 CON ULTIMO
042800000000     C** GIORNO DEL MESE
042900011130     C******************************************************
043000020704     C     CARCON        BEGSR
043100000000     C*CONSUMO OLIO
043200020704     C                   MOVEL     '£2'          INT
043300020704     C     KGAI3         CHAIN     mzIMP000                           23
043400020704     C   23              Z-ADD     0             SC1COL
043500020704     C  N23              Z-ADD     IMPQTA        SC1COL
043600020704     C     VNRF          CHAIN     mzAME1                             24
043700160518     c                   if        %found(mzame01l)
043800160518     c                   movel     'MAU'         tbecod
043900160518     c                   movel(p)  amemau        tbeke1
044000160518     c     ktbe          chain     tntbe01l
044100160518     c                   if        %found(tntbe01l)
044200160518     c                   movel     tbeuni        dmau
044300160518     c                   movel     §maudes       amemaud
044400160518     c                   endif
044500160518     c                   movel     'CAU'         tbecod
044600160518     c                   movel(p)  amecaut       tbeke1
044700160518     c     ktbe          chain     tntbe01l
044800160518     c                   if        %found(tntbe01l)
044900160518     c                   movel     tbeuni        dcau
045000160518     c                   movel     §caudes       amecautd
045100160518     c                   endif
045200160518     c                   endif
045300020704     C                   ENDSR
045400000000     C******************************************************
045500011130     C** CONTROLLI SUBF1
045600011130     C******************************************************
045700000000     C     CONSF1        BEGSR
045800011203     C                   CLEAR                   PR
045900021022     C                   clear                   obq
046000020730     c                   setoff                                       995257
046100020730     c                   setoff                                       5859
046200011130     C                   DO        *HIVAL        A                 4 0
046300000000     C     A             CHAIN     GA23DSF1                           05
046400011130     C   05              leave
046500020730     c* controllo che ci sia selezionato almeno un gg (se no rimorchio)
046600020730     c                   if        §xarsm <> 'S'
046700020705     c                             and ggds = *blanks
046800020730     c                   select
046900020730     c                   when      sf1cin = '£1'
047000020730     C                   SETON                                        58
047100020730     c                   when      sf1cin = 'CB'
047200020730     C                   SETON                                        59
047300020730     C                   ENDsl
047400020705     C                   ENDif
047500020703     c* controllo ultimo gg del mese
047600020730     C                   DO        31            B                 2 0
047700000000     C     GG(B)         IFNE      *BLANKS
047800011130     C     B             IFGT      salgg
047900011130     C                   SETON                                        5299
048000011130     C   52              leave
048100011130     C                   ENDif
048200910214     C     SF1UNI        IFEQ      'KM'
048300000000     C                   MOVEL     GG(B)         PR(B)
048400011130     C                   ENDif
048500021022     c                   if        sf1tip <> ' '
048600021022     C                   MOVE      sf1tip        obq(B)
048700011130     C                   ENDif
048800021022     C                   ENDif
048900011130     C                   ENDdo
049000011203     C                   UPDATE    GA23DSF1
049100020730     c                   if        *in52
049200011130     c                   leave
049300011130     C                   ENDif
049400020730     C                   ENDdo
049500020730     c* se non inserito almeno un gg di carburante errore
049600020730     c                   if        *in58 and *in59
049700020730     C                   SETON                                        5799
049800020730     C                   ENDif
049900011130     C*
050000011130     C                   ENDSR
050100000000     C******************************************************
050200011130     C** CARICAMENTO SUBF2
050300011130     C******************************************************
050400000000     C     CARSF2        BEGSR
050500000000     C                   Z-ADD     0             NR1               4 0
050600000000     C                   DO        31            A
050700000000     C                   Z-ADD     A             SF2GG
050800000000     C     PR(A)         IFEQ      *BLANKS
050900000000     C                   Z-ADD     0             SF2KM1
051000020704     C                   Z-ADD     0             SF2cca
051100021031     C                   SETON                                        7880
051200000000     C                   ELSE
051300021031     C                   SETOFF                                       7880
051400000000     C                   Z-ADD     KM(A)         SF2KM1
051500021022     c                   if        obq(a) = ' '
051600021031     c                   seton                                        80
051700021022     c                   z-add     0             sf2cca
051800021022     c                   else
051900021031     c                   setoff                                       80
052000020703     C                   Z-ADD     qt(A)         SF2cca
052100021022     c                   end
052200021022     C                   move      obq(a)        SF2tip
052300020703     C                   END
052400000000     C                   ADD       1             NR1
052500000000     C                   WRITE     GA23DSF2
052600000000     C                   END
052700011130     C                   Z-ADD     NR1           SVNRR             4 0
052800000000     C                   ENDSR
052900000000     C******************************************************
053000011130     C** CONTROLLI SUBF2
053100011130     C******************************************************
053200000000     C     CONSF2        BEGSR
053300020703     c*
053400011130     C                   CLEAR                   KM1
053500020703     C                   CLEAR                   prkm
053600021031     C                   SETOFF                                       809978
053700011130     C                   DO        *HIVAL        A                 4 0
053800000000     C                   READC     GA23DSF2                               06
053900011130     C   06              leave
054000020704     c                   setoff                                       55
054100020703     c* controllo km
054200000000     C     SF2KM1        IFEQ      0
054300020704     C                   SETON                                        559979
054400000000     C                   ELSE
054500020704     C                   SETOFF                                           79
054600020705     C                   END
054700020703     c* controllo che ci sia congruenza nei km immessi
054800000000     C     SF2KM1        IFLT      KM1
054900020704     C                   SETON                                        559954
055000000000     C                   END
055100020703     c* controllo che ci sia congruenza nei km immessi nei mesi precedenti
055200020703     C                   IF        prkm = *blanks
055300020703     c                   eval      prkm = '1'
055400020703     C     SF2KM1        IFLT      KMp
055500020704     C                   SETON                                        559953
055600020703     C                   END
055700020703     C                   END
055800020703     c* controllo qta
055900021022     c                   if        sf2tip = '1'
056000021031     c                   setoff                                       80
056100020703     C     SF2cca        IFEQ      0
056200020703     C                   SETON                                          9977
056300020703     C                   ELSE
056400020703     C                   SETOFF                                           77
056500020703     C                   END
056600021031     C                   ELSE
056700021031     c                   seton                                        80
056800020703     C                   END
056900000000     C                   UPDATE    GA23DSF2
057000000000     C                   Z-ADD     SF2KM1        KM1               7 0
057100000000     C                   Z-ADD     SF2KM1        KM(NR1)
057200020703     C                   Z-ADD     SF2cca        qt(NR1)
057300000000     C  N99              END
057400020704     c* controllo che ci sia congruenza nei km immessi nei mesi successivi
057500020704     C     SF2KM1        IFgT      KMd
057600020704     c     kmd           andgt     0
057700021031     c     nr1           chain     ga23dsf2                           81
057800021031     c                   if        not *in81
057900021031     c                   if        sf2tip = '1'
058000021031     c                   setoff                                       80
058100021031     C                   ELSE
058200021031     c                   seton                                        80
058300021031     C                   END
058400020704     C                   SETON                                        559956
058500020704     c                   update    ga23dsf2
058600020704     C                   END
058700020704     C                   END
058800011130     C                   ENDSR
058900000000     C******************************************************
059000020704     C** reperisce km del mese precedente e del mese successivo
059100011130     C******************************************************
059200020703     C     srkm          BEGSR
059300020703     c*
059400020703     C                   CLEAR                   KMp               7 0
059500020703     C                   Z-ADD     DATIMP        IMPDAT
059600020703     C                   MOVE      00            IMPDAT
059700020703     c     kimp          setgt     mzimp03l
059800020703     c                   do        *hival
059900020703     c     vnrf          readpe    mzimp03l
060000020703     c                   if        %eof(mzimp03l)
060100020703     c                   leave
060200020703     c                   end
060300020703     c                   z-add     impkm         kmp
060400020703     c                   if        kmp <>0
060500020703     c                   leave
060600020703     c                   end
060700020703     c                   enddo
060800020704     c*
060900020704     C                   CLEAR                   KMd               7 0
061000020704     C                   Z-ADD     DATIMP        IMPDAT
061100020704     C                   MOVE      31            IMPDAT
061200020704     c     kimp          setgt     mzimp03l
061300020704     c                   do        *hival
061400020704     c     vnrf          reade     mzimp03l
061500020704     c                   if        %eof(mzimp03l)
061600020704     c                   leave
061700020704     c                   end
061800020704     c                   z-add     impkm         kmd
061900020704     c                   if        kmd <>0
062000020704     c                   leave
062100020704     c                   end
062200020704     c                   enddo
062300020703     c*
062400020703     C                   ENDSR
062500020703     C******************************************************
062600020703     C** AGGIORNAMENTO mzIMP
062700020703     C******************************************************
062800020703     C     AGGAIM        BEGSR
062900020704     c                   z-add     0             salkm
063000011130     C                   DO        *HIVAL        A
063100000000     C     A             CHAIN     GA23DSF1                           05
063200011130     C   05              leave
063300000000     C                   DO        31            B
063400000000     C                   Z-ADD     DATIMP        IMPDAT
063500000000     C                   MOVE      B             IMPDAT
063600011210     C     KGAI          DELETE    mzIMP000                           08
063700000000     C     GG(B)         IFNE      *BLANKS
063800000000     C                   Z-ADD     VNRF          IMPNRF
063900000000     C                   MOVEL     SF1CIN        IMPINT
064000020703     C                   Z-ADD     KM(B)         IMPKM
064100020704     c                   if        impkm <> 0 and impkm > salkm
064200020704     c                   z-add     impkm         salkm
064300020704     c                   end
064400020703     c                   if        sf1tip = ' '
064500000000     C                   Z-ADD     0             IMPQTA
064600020703     c                   else
064700020703     C                   Z-ADD     qt(b)         IMPQTA
064800020703     c                   end
064900011210     C                   WRITE     mzIMP000
065000011130     C                   ENDif
065100011130     C                   ENDdo
065200011130     C                   ENDdo
065300000000     C** CONSUMO OLIO
065400020704     C                   MOVEL     '£2'          INT
065500020704     C     KGAI3         CHAIN     mzIMP000                           08
065600020704     C                   Z-ADD     VNRF          IMPNRF
065700020704     C                   MOVEL     INT           IMPINT
065800020704     C                   Z-ADD     DATIMP        IMPDAT
065900020704     C                   Z-ADD     salkm         IMPKM
066000020704     C                   Z-ADD     SC1COL        IMPQTA
066100020704     C   08              WRITE     mzIMP000
066200020704     C  N08              UPDATE    mzIMP000
066300011130     C                   ENDSR
066400011130     C*---------------------------------------------------------------*
066500011130     c     *inzsr        begsr
066600011130     C*---------------------------------------------------------------*
066700011130     C     *ENTRY        PLIST
066800011130     C                   PARM                    KPJBA
066900011130     C                   Z-ADD     1             CODUT
067000020703     C                   CALL      'X§PARUT'
067100020703     C*                  PARM                    UTEDtE
067200020703     C                   PARM                    UT§DSE0F
067300011130     C                   IF        CODUT = -1
067400011130     C                   SETON                                        LR
067500011130     C                   END
067600011130     C                   MOVEL     RAGUT         RSUT             20
067700011130     C                   MOVEL     RAGUT         RSUT2            25
067800011130     C*----------------------------------------------------*
067900011130     C* POSIZIONAMENTO TABELLE                             *
068000011130     C     KTAB          KLIST                                                  *
068100011130     C                   KFLD                    CODUT                          *
068200011130     C                   KFLD                    COD               2            *
068300011130     C     KTAB1         KLIST                                                  *
068400011130     C                   KFLD                    CODUT                          *
068500011130     C                   KFLD                    COD               2            *
068600011130     C                   KFLD                    KEY               8            *
068700160518      *
068800160518     C     KTbe          KLIST                                                  *
068900160518     C                   KFLD                    tbecod                         *
069000160518     C                   KFLD                    tbeke1                         *
069100011210     C* POSIZIONAMENTO mzIMP000                            *
069200011130     C     KGAI          KLIST                                                  *
069300011130     C                   KFLD                    VNRF                           *
069400011130     C                   KFLD                    SF1CIN                         *
069500011130     C                   KFLD                    IMPDAT                         *
069600011210     C* LETTURA DI     mzIMP000                            *
069700011130     C     KGAI2         KLIST                                                  *
069800011130     C                   KFLD                    VNRF                           *
069900011130     C                   KFLD                    SF1CIN                         *
070000011130     C* CARICAMENTO CONS.CARB.CONS.OLIO                    *
070100011130     C     KGAI3         KLIST                                                  *
070200011130     C                   KFLD                    VNRF                           *
070300011130     C                   KFLD                    INT               2            *
070400011130     C                   KFLD                    DATIMP            8 0          *
070500020703     C     Kimp          KLIST                                                  *
070600020703     C                   KFLD                    VNRF                           *
070700020703     C                   KFLD                    impdat                         *
070800011210     C* LETTURA DI     mzTIM000                            *
070900011130     C     KTIM          KLIST                                                  *
071000011130     C                   KFLD                    TIMTIP                         *
071100011130     C                   KFLD                    TIMINT                         *
071200011130     C                   KFLD                    TIMMAN                         *
071300011130     C*
071400011130     C* CHAIN X N.DI TARGA                                 *
071500011130     C     KTAR          KLIST                                                  *
071600011130     C                   KFLD                    VTGA                           *
071700011130     C                   KFLD                    VTGN                           *
071800011130     C*
071900011130     C* RICERCA TARGA X CITTA'
072000011130     C     RICTAR        PLIST
072100011130     C                   PARM                    CODUT
072200011130     C                   PARM                    RSUT
072300011130     C                   PARM                    VTGA
072400011130     C                   PARM                    VTGN
072500011130     C*
072600011130     c                   endsr
