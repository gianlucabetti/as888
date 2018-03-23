000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200160427     h dftactgrp(*no) actgrp(*caller)
000300061222     H* FIDG01R *-----------------------------------------------------*
000400060929     H*            GESTIONE DISTINTE CONSEGNA
000500930225     H*---------------------------------------------------------------*
000600061222     FFidg01D   CF   E             WORKSTN
000700950525     F                                     SFILE(LRF0S03:NRR)
000800070410     Ffidst09l  UF A E           K DISK    commit
000900061222     FFidst31L  UF A E           K DISK    commit
001000070717     FFidst51L  UF A E           K DISK    commit
001100070410     Ffidst08l  IF   E           K DISK
001200061228     F                                     RENAME(FIdst000:FIdst007)
001300080326     Ffidsf02l  iF   E           K DISK
001400020910     FFNARB78L  IF   E           K DISK
001500070223     Ffiarg03L  IF   E           K DISK
001600070131     FFnBRV01L  IF   E           K DISK
001700011121     FFIFTT02L  IF   E           K DISK
001800011121     FFIFST02L  IF   E           K DISK
001900011121     F                                     RENAME(FIFTT000:FIFST000)
002000021203     Ffiapd01L  IF   E           K DISK
002100061222     Ffiapd22L  IF   E           K DISK
002200061222     Ffiapd31L  IF   E           K DISK
002300070719     Ffiapd51L  IF   E           K DISK
002400020419     FTntbe01l  IF   E           K DISK
002500950525     FAZORG01L  IF   E           K DISK
002600010314     FFNORM06L  IF   E           K DISK
002700110223     FFipct02L  IF   E           K DISK
002800061003     FFiqdt01L  UF A E           K DISK    commit
002900950529     FFNLV22P   O    F  132        PRINTER
003000020910
003100020910     d karbifp         s                   Like(Arbifp)
003200020910     d karbndc         s                   Like(Arbndc)
003300070223     d kargfgs         s                   Like(Argfgs)
003400070223     d kargndc         s                   Like(Argndc)
003500030724     d dtaiso          s               d
003600030724     d dtaeur          s               d   datfmt(*eur)
003700061222     d wdtiso1         s               d
003800030917     d wdtiso2         s               d
003900070726     d yy              s              1  0
004000950524     D*
004100950524     D* DEFINIZIONE SCHIERE
004200170206     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             MESSAGGI ERRORE
004300170207     D PDR             S              7  0 DIM(9999)                            PAD CON DIST  E
004400950523     D*
004500950908     D* PASSO DA FNLRF0C PGM STAMPA ETICHETTE DISTINTA
004600950908     D PARAM1          DS
004700070424     D  PA1fgs                 1      3
004800950908     D* PGM STAMPA ETICHETTE DISTINTA
004900950908     D  PA1PSD               247    256
005000950523     D*
005100061002     D fidstds       E DS                  INZ extname(fidst00f)
005200061002     D fiapd2ds      E DS                  extname(fiapd20f)
005300061002     D OG150         E DS                  INZ
005400061010     D OG146         E DS                  INZ
005500080208     D OG148         E DS                  INZ
005600070723     D OG147         E DS                  INZ
005700050519     D* DS PER APDFLR  - filler anagrafica  autotrasp.
005800050519     D dapdflr       E DS
005900080207     D* DS PER DSTFLR  - filler distinte
006000080207     D ddstflr       E DS
006100050519     D* DS PER FNLV24R - ricerca autotrasp.
006200050519     D fnlv24ds      E DS
006300061222     D fidg02ds      E DS
006400061229     D fidg05ds      E DS
006500070717     D fidg09ds      E DS
006600021203     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
006700021203     D DSLV50        E DS                  EXTNAME(FNLV50DS)
006800160720     D* DS PER Fidg31R - INTERROGAZIONE FOGLI VARI
006900160720     D fidg31ds      E DS
007000950529     D* DS PER FNLV25R - STAMPA ETICHETTA PADRONCINI
007100950529     D DSLV25        E DS                  EXTNAME(FNLV25DS)
007200980430     D**
007300020419     d ddgd          e ds
007400950908RDE  D*
007500030724RDE  D* DS PER FNLR59R - quadratura DISTINTA MANUALE e da spunte (x poste)
007600030724RDE  D FNLR59ds      E DS
007700020910
007800020910     d Trul33Ds      e ds
007900061227     d Fidg03ds      e ds
008000020910
008100950524     D*
008200950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008300950524     D WLBDAT          DS                  INZ
008400950524     D  G02DAT                 1      8  0
008500950524     D  G02INV                 9     16  0
008600950524     D  G02ERR                17     17
008700950524     D  G02TGI                18     22  0
008800020419      *
008900020419     d Wdata8          DS
009000020419     d  dadata                 1      8  0
009100020419     d  adata                  9     16  0
009200020419     d  GioLav                17     21  0
009300061002     d* schiere file fiapd20f
009400061002     D Ptc1            S               *
009500061002     D                                     INZ(%ADDR(apd2tc1))
009600061002     D tc1             S                   LIKE(apd2tc1)
009700061002     D                                     DIM(5)
009800061002     D                                     BASED(Ptc1)
009900061002     D Pts1            S               *
010000061002     D                                     INZ(%ADDR(apd2ts1))
010100061002     D ts1             S                   LIKE(apd2ts1)
010200061002     D                                     DIM(2)
010300061002     D                                     BASED(Pts1)
010400061002     d* schiere file fidst00f
010500061002     D Ptc1d           S               *
010600061002     D                                     INZ(%ADDR(dsttc1))
010700061002     D tc1d            S                   LIKE(dsttc1)
010800061002     D                                     DIM(5)
010900061002     D                                     BASED(Ptc1d)
011000061002     D Pts1d           S               *
011100061002     D                                     INZ(%ADDR(dstts1))
011200061002     D ts1d            S                   LIKE(dstts1)
011300061002     D                                     DIM(2)
011400061002     D                                     BASED(Pts1d)
011500950525     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRGI8 -
011600950525     D WGIDAT          DS                  INZ
011700950525     D  GIODAT                 1      8  0
011800950525     D  GIOINV                 9     16  0
011900950525     D  GIOTGI                17     21  0
012000950523     D*
012100950525     D                 DS
012200950525     D  AA                     1      4  0
012300950525     D  MM                     5      6  0
012400950525     D  GG                     7      8  0
012500950525     D  WDATA                  1      8  0
012600950523     D                 DS
012700950523     D  V1CFPD                 1      3  0
012800950523     D  V1CCPC                 4      7
012900950523     D  WPDR                   1      7
013000950523     D*
013100950524     D DS1C          E DS
013200900514     D KPJBA         E DS
013300170206     D*
013400170206     d   savpjbu       s                   like(kpjbu)
013500170206     D fnlr50wDS     E DS
013600170206     D*
013700061002     d* Parametri x Controllo profilo utenti
013800061002     d TIBS34ds      e ds
013900061002     d AZUTEds       e ds                  extname(AZUTE00F)
014000160720     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
014100061002     d* Ds per dati organigramma
014200061002     d DDatiUte      e ds
014300061011     D TIBS02        E DS                  EXTNAME(TIBS02DS)
014400010314     D DFAR          E DS
014500010314     D  §FARDS       E                     EXTFLD(D§FARDES)
014600010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
014700010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
014800010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
014900010314     D  §FARST       E                     EXTFLD(D§FARSTP)
015000010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
015100010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
015200950526     D*
015300950525     D* DEFINIZIONE COSTANTI
015400940712     D CINSER          C                   CONST(' Inserimento ')
015500940712     D CVARIA          C                   CONST(' Variazione  ')
015600940712     D CANNUL          C                   CONST('  Annullato  ')
015700160427     d Put             C                   1
015800160427     d EndPut          C                   2
015900160426     D Ordinamento     S              1
016000160427     d Numerico        C                   8
016100160427     d Carattere       C                   6
016200160427     d Ascendente      C                   1
016300160427     d Get             C                   3
016400160427     d NotUsed         S             16A
016500160427     d ReturnSize      S              9B 0
016600160427     d SizeList        S              9B 0
016700160428     d MaxKey          C                   2
016800160427     d stmrec1         S              4  0
016900160426      *  1=Autista
017000160426      *  2=RagSociale
017100160426      *  3=turno/codice
017200160426      ***
017300160426     d SflRcd          DS
017400160426     d  VSCPDR
017500160426     d  VSDPDR
017600160426     d  VSCTUR
017700160428     d  VSCTPM
017800160426     d  VSCFPP
017900160426     d  VSCPEP
018000160426     d  VSCDFV
018100160426     d  VSCPDA
018200160426     d  VSCTSTPDA
018300160428     d  VHRDFV
018400160428     d  VHCDFV
018500160428     d  VSCSCE
018600160426     d  VSCDSN
018700170207     d  hPdrForza
018800160426     d  Selected                      1A
018900160426
019000160426      /COPY QSYSINC/QRPGLESRC,QLGSORT
019100160426     d QLGKL                         16    DIM(MaxKey)
019200160426     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
019300160426     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
019400160426     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
019500160426     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
019600160426      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
019700160426      /COPY QSYSINC/QRPGLESRC,QUSEC
019800061002     C**********************************************************************
019900061002     C*
020000950523     C* WGES = 'F' ===> VADO A FINE PGM
020100950523     C     WGES          DOWNE     'F'
020200950523     C*
020300950523     C* WGES = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
020400950523     C     WGES          IFEQ      '1'
020500950523     C                   EXSR      GESD01
020600950523     C                   ENDIF
020700950523     C*
020800950524     C* WGES = '2' ===> GESTIONE SINGOLA VIDEATA DATI FOGLIO DISTINTA
020900950523     C     WGES          IFEQ      '2'
021000950523     C                   EXSR      GESD02
021100950523     C                   ENDIF
021200950523     C*
021300950523     C* WGES = '3' ===> GESTIONE VIDEATA SUBFILE DI TUTTE LE DISTINTE
021400950523     C     WGES          IFEQ      '3'
021500950523     C                   EXSR      GESS03
021600950523     C                   ENDIF
021700950523     C                   ENDDO
021800010208      *
021900020910
022000020910     c                   Clear                   Trul33Ds
022100020910     c                   Eval      I33Tla = 'C'
022200020910     c                   Movel(p)  Trul33Ds      Kpjbu
022300020910     c                   Call      'TRUL33R'
022400020910     c                   Parm                    Kpjba
022500020910
022600061227     c                   Clear                   Fidg03ds
022700061227     c                   Eval      D3Tla = 'C'
022800061227     c                   Movel(p)  Fidg03ds      Kpjbu
022900061227     c                   Call      'FIDG03R'
023000020910     c                   Parm                    Kpjba
023100070301      * MI RIPORTO IL NUMERO DISTINTA
023200070301     C     V1CNFP        IFGT      *ZEROS
023300070301     C                   CLEAR                   KPJBU
023400070301     C                   MOVEL     V1CNFP        KPJBU
023500070301     C                   ENDIF
023600950523     C*
023700950523     C                   SETON                                        LR
023800060929     C**********************************************************************
023900950523     C*--- GESTIONE VIDEATA DELLE SELEZIONI --------------------------*
024000060929     C**********************************************************************
024100950523     C     GESD01        BEGSR
024200950525     C                   SETOFF                                       010203
024300070515     C                   SETOFF                                       060908
024400950524     C                   CLEAR                   WUNO
024500050519     C                   CLEAR                   V05dfv
024600950525     C                   CLEAR                   V1CNFV
024700950525     C                   CLEAR                   V1CCPC
024800950523     C*
024900950524     C* EMISSIONE FORMATO
025000060929     C                   do        *hival
025100950523     C                   EXFMT     LRF0D01
025200950523     C*
025300950523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
025400950523     C                   CLEAR                   V1CMSG
025500950523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
025600950523     C                   SETOFF                                       2840
025700950525     C                   SETOFF                                       414243
025800950523     C*
025900950523     C** CMD3  - FINE LAVORO
026000950526    1C     *INKC         IFEQ      *ON
026100950526     C                   MOVEL     'F'           WGES
026200060929     C                   leave
026300950526    1C                   ENDIF
026400950523     C*
026500950523     C** CMD18 - CAMBIO FILIALE
026600950526    1C     *INKS         IFEQ      *ON
026700950526     C                   SETON                                        04
026800060929     C                   iter
026900950526    1C                   ENDIF
027000950523     C*
027100950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
027200950524    1C     *IN04         IFEQ      *ON
027300950523     C                   MOVEL     V1CFGS        WFIL
027400950523     C                   EXSR      CTRFGS
027500060929     C   28              iter
027600950524    1C                   ENDIF
027700950523     C*
027800950523     C* CONTROLLI VIDEATA
027900950523     C                   EXSR      CTRD01
028000950524     C   28
028100060929     COR 90              iter
028200950524     C*
028300950524     C** CMD15 - INSERIMENTO MULTIPLO
028400950524    1C     *INKP         IFEQ      *ON
028500050519     c                   exsr      gesd05
028600950525     C                   SETON                                        03
028700950524     C                   MOVEL     '3'           WGES
028800060929     C                   leave
028900950524    1C                   ENDIF
029000950524     C*
029100950525     C* SE E' STATO INSERITO IL NUMERO DISTINTA IN PRIMA VIDEATA(02 ON)
029200950525     C*   PASSO ALLA GESTIONE DELLA VIDEATA DEL SINGOLO FOGLIO DISTINTA
029300950525    1C     *IN02         IFEQ      *ON
029400950524     C                   MOVEL     '2'           WGES
029500060929     C                   leave
029600950524    1C                   ENDIF
029700950526     C*
029800950526     C** ENTER - IMMISSIONE
029900950526     C                   SETON                                        01
030000060929     c                   MOVEL     '2'           WGES
030100060929     c                   leave
030200060929     c                   enddo
030300950526     C*
030400060929     C                   ENDSR
030500060929     C**********************************************************************
030600950523     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
030700060929     C**********************************************************************
030800950523     C     CTRFGS        BEGSR
030900950523     C                   CLEAR                   DSLV50
031000950523     C                   MOVEL     KNMUS         D50PRU
031100950523     C                   MOVEL     WFIL          D50FGS
031200950523     C                   CALL      'FNLV50R'
031300950523     C                   PARM                    DSLV50
031400950523     C*
031500950524    1C     D50ERR        IFNE      ' '
031600950523     C                   MOVEL     D50MSG        V1CMSG
031700950525     C                   SETON                                        4328
031800950524    1C                   ENDIF
031900950523     C*
032000060929     C                   ENDSR
032100060929     C**********************************************************************
032200950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
032300060929     C**********************************************************************
032400950523     C     CTRD01        BEGSR
032500950525     C                   SETOFF                                       010203
032600950525     C                   SETOFF                                       049015
032700950525     C                   CLEAR                   WBOL
032800960904     C                   CLEAR                   WSPU
032900010314     C                   CLEAR                   WORM
033000080320     C                   CLEAR                   faseorm           3
033100050519     c* ind 50 per video se p.o. gestisce Parcel+Messaggerie
033200050519     C     V1CFGS        CHAIN     AZORG01l
033300050519    1C                   IF        %found(azorg01l)
033400050519     C                   MOVEL     ORGdf0        og150
033500061010     C                   MOVEL     ORGde6        og146
033600080208     C                   MOVEL     ORGde8        og148
033700070723     C                   MOVEL     ORGde7        og147
033800080212     c* se procedura PDA non partita proteggo i flag
033900080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
034000080212     c                   setoff                                       96
034100080212     c                   else
034200080212     c                   seton                                        96
034300080212     c                   end
034400080212     c* ind. 11 x visualizzare i valori assegnati se EDP
034500080212     c                   if        %subst(knmus: 1: 3) = 'EDP'
034600080212     c                   seton                                        11
034700080212     c                   else
034800080212     c                   setoff                                       11
034900080212     c                   end
035000061010     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
035100061010     c* se data distinte automatiche < oggi p.o. partito
035200061010     c                   if        §ogdda = ' '
035300061010     c                   seton                                        lr
035400061010     c                   return
035500061010     c                   else
035600070109     c                   move      §ogdda        comdda            8 0
035700070330     c* ind. 11 x visualizzare i valori assegnati se EDP
035800070330     c                   if        %subst(knmus: 1: 3) = 'EDP'
035900070328     c                   seton                                        11
036000070328     c                   else
036100070328     c                   setoff                                       11
036200061010     c                   end
036300070328     c                   end
036400050519     c     §OGPKM        comp      'S'                                    50
036500160426     c     §OGturno      comp      'S'                                    54
036600050519   X1C                   ELSE
036700160426     c                   setoff                                       5054
036800050519    1C                   ENDIF
036900160426     c                   eval      vtesta ='1 |  AUTOTRASPORTATORE            -
037000160505     c                                       |M/P|Picking|Data dist.| -
037100160426     c                                       '
037200160505     c   50              eval      %subst(vtesta: 37: 4)='|P/M'
037300160505     c   54              eval      %subst(vtesta: 41: 4)='|Tur'
037400160426     c   96              eval      %subst(vtesta: 69: 8)='PDA TEST'
037500950607     C*
037600060929     c                   do
037700950607     C****  CODICE PADRONCINO  ****
037800950607     C* RICERCA
037900950607     C     '?'           SCAN      V1CCPC                                 90
038000950607    1C     *IN90         IFEQ      *ON
038100021203     c                   clear                   fnlv24ds
038200021203     C                   Z-ADD     V1CFGS        d24FIL
038300021203     C                   MOVEL     'R'           d24FLG
038400021203     C                   MOVEL     'A'           d24tip
038500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
038600950607     C                   CALL      'FNLV24R'
038700950607     C                   PARM                    KPJBA
038800021203     C                   MOVEL     KPJBU         fnlv24ds
038900950607     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
039000021203     C     d24pdr        IFNE      0
039100021203     C                   MOVEL     d24pdr        WPDR
039200021203     C                   MOVEL     d24rsc        V2DPDR
039300950607     C                   ELSE
039400950607     C                   CLEAR                   V1CCPC
039500950607     C                   ENDIF
039600060929     C                   leave
039700950607    1C                   ENDIF
039800950607     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
039900950607    1C     V1CCPC        IFNE      *BLANKS
040000950607     C     V1CCPC        ANDNE     *ZEROS
040100950607     C                   TESTN                   V1CCPC               35
040200950607    2C     *IN35         IFEQ      *ON
040300950607     C                   MOVE      V1CCPC        W001              1
040400950607     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
040500950607    3C     W001          IFLT      '0'
040600950607     C                   SETOFF                                       35
040700950607    3C                   ENDIF
040800950607    2C                   ENDIF
040900950607    2C     *IN35         IFEQ      *OFF
041000950607     C                   MOVEL     MSG(10)       V1CMSG
041100950607     C                   SETON                                        4128
041200060929     C                   leave
041300950607    2C                   ENDIF
041400950607     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
041500950607     C                   MOVE(P)   WPDR          WPADR
041600061002     C     kapdw         CHAIN     fiapd01l
041700061002     c                   if        not %found(fiapd01l) or
041800061002     c                             (%found(fiapd01l) and apdatb <> ' ')
041900950607     C                   MOVEL     MSG(10)       V1CMSG
042000950607     C                   SETON                                        4128
042100060929     C                   leave
042200070123     c                   else
042300070123     c                   eval      dapdflr = apdflr
042400950607    1C                   ENDIF
042500061003    1C                   ENDIF
042600950523     C****  NUMERO DISTINTA  ****
042700950523     C* RICERCA
042800950523     C     '?'           SCAN      V1CNFV                                 90
042900950523    1C     *IN90         IFEQ      *ON
043000160720     C                   CLEAR                   fidg31ds
043100160720     C                   Z-ADD     0             fdggda
043200160727     C                   Z-ADD     0             fdggal
043300160720     C                   Z-ADD     V1CFGS        fdgFGS
043400160720     C                   MOVEL     'FIDG01R'     fdgPGM
043500160720     C                   MOVEL     'S'           fdgRIC
043600160720     C                   MOVEL     '2'           fdgTFV
043700160720     C                   Z-ADD     4             fdgNPG
043800160720     C                   MOVEL     fidg31ds      KPJBU
043900160720     c                   clear                   $FV6
044000160720      * impostato a tutti 99999
044100160720     c                   do        600           zz                3 0
044200160720     c                   z-add     999999        $FV6(zz)
044300160720     c                   enddo
044400160720     C                   CALL      'FIDG31R'
044500061228     C                   PARM                    KPJBA
044600160720     c                   parm                    $FV6
044700160720     C                   MOVEL     KPJBU         fidg31ds
044800070627     c                   movel     *all'0'       v1cnfv
044900160720     C                   move      fdgNFV        v1cnfv
045000060929     C                   leave
045100950523    1C                   ENDIF
045200950523     C*
045300950523    1C     V1CNFV        IFNE      *BLANKS
045400950523     C     V1CNFV        ANDNE     *ZEROS
045500950523     C                   TESTN                   V1CNFV               35
045600950523    2C     *IN35         IFEQ      *ON
045700950523     C                   MOVE      V1CNFV        W001
045800950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
045900950523    3C     W001          IFLT      '0'
046000950523     C                   SETOFF                                       35
046100950523    3C                   ENDIF
046200950523    2C                   ENDIF
046300950523    2C     *IN35         IFEQ      *OFF
046400950524     C                   MOVEL     MSG(1)        V1CMSG
046500950524     C                   SETON                                        4028
046600060929     C                   leave
046700950523    2C                   ENDIF
046800061002     C* SE NUMERO DISTINTA >< 0 CONTROLLO SE ESISTE IN Fidst00F
046900061010     C                   MOVE      V1CNFV        WNFV
047000070410     C     Kdst          CHAIN     fidst09l                             39
047100950523     C* 39 ON  - RECORD ALLOCATO
047200950524    2C     *IN39         IFEQ      *ON
047300950523     C                   MOVEL     MSG(2)        V1CMSG
047400950524     C                   SETON                                        4028
047500060929     C                   leave
047600950524    2C                   ENDIF
047700950523     C*
047800070410    2C                   IF        %found(fidst09l)
047900080207     c                   movel     dstflr        ddstflr
048000061227     C* FOGLIO DISTINTA GIA' CHIUSO
048100061227    3C     dstFCF        IFEQ      'S'
048200061227     C                   MOVEL     MSG(3)        V1CMSG
048300061227     C                   SETON                                        4028
048400061227     C                   leave
048500061228   X3C                   ELSE
048600061228    4C     dstFCF        IFNE      ' '
048700950524     C* FOGLIO DISTINTA NON MANUTENZIONABILE
048800061228     C                   MOVEL     MSG(4)        V1CMSG
048900061228     C                   SETON                                        4028
049000061228     C                   leave
049100061228    4C                   ENDIF
049200061228    3C                   ENDIF
049300080319     C*fasi distinta
049400080319     c                   exsr      srfasi
049500950525     C* 06 ON  - FOGLIO DISTINTA ANNULLATO: ABILITO CMD5-RIATTIVAZIONE
049600950525     C     DSTATB        IFNE      ' '
049700950525     C                   MOVEL     CANNUL        V1CDES
049800950525     C                   SETON                                        06
049900950530     C* SE FOGLIO DISTINTA ANNULLATO NON CONSIDERO IL PADRONCINO
050000950530     C                   CLEAR                   V1CCPC
050100950529     C                   ELSE
050200950529     C                   MOVEL     CVARIA        V1CDES
050300950525     C                   ENDIF
050400950525   X3C                   ELSE
050500950524     C* NUMERO DISTINTA INESISTENTE
050600950524     C                   MOVEL     MSG(5)        V1CMSG
050700950524     C                   SETON                                        4028
050800060929     C                   leave
050900950524    3C                   ENDIF
051000060929     C* SE esiste Fiqdt00F
051100070109     C     Kqdt          CHAIN(n)  Fiqdt01l                           27
051200950524     C* CONTROLLO SE ESISTONO BOLLE SUL FOGLIO DISTINTA DIGITATO
051300070312     c                   setoff                                       31
051400070424     c                   if        comdda= 20391231 or comdda > dateu8
051500070223     c                   Z-Add     V1cfgs        Kargfgs
051600070223     C                   Z-ADD     WNFV          Kargndc
051700070223     C     Karg          SETLL     Fiarg03l                               31
051800070312     c                   end
051900070312     c                   if        not *in31
052000070223     c                   Z-Add     V1cfgs        Karbifp
052100070223     C                   Z-ADD     WNFV          Karbndc
052200070223     C     Karb78        SETLL     FNARB78L                               31
052300070223     c                   end
052400950525     C   31              MOVEL     'S'           WBOL              1
052500070410     C     Kbrv          SETLL     FnBRV01L                               31
052600960904     C   31              MOVEL     'S'           WSPU              1
052700010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
052800080319    2C*    WBOL          IFEQ      *BLANKS
052900080319     C*    WSPU          ANDEQ     *BLANKS
053000010314     C     KORM          SETLL     FNORM06L
053100010314     C     KORM          READE     FNORM06L                               31
053200010314    3C     *IN31         DOWEQ     *OFF
053300010314     C                   CLEAR                   TIBS02
053400010314     C                   CLEAR                   DFAR
053500010314     C                   MOVEL     'C'           T02MOD
053600010314     C                   MOVEL     KNSIF         T02SIF
053700010314     C                   MOVEL     'FAR'         T02COD
053800010314     C                   MOVEL     ORMFAO        T02KE1
053900010314     C                   CALL      'TIBS02R'
054000010314     C                   PARM                    KPJBA
054100010314     C                   PARM                    TIBS02
054200010314     C     T02ERR        IFEQ      *BLANKS
054300010314     C                   MOVEL     T02UNI        DFAR
054400010314     C                   ENDIF
054500010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
054600010314     C* DISTINTA E' PRESENTE UN O.R.M.
054700010314    4C     §FARAF        IFNE      'S'
054800010314     C                   SETON                                        31
054900010314     C                   MOVEL     'S'           WORM              1
055000010314   X4C                   ELSE
055100010314     C     KORM          READE     FNORM06L                               31
055200010314    4C                   ENDIF
055300010314    3C                   ENDDO
055400080319    2C*                  ENDIF
055500950524     C*
055600950524    2C     V1CCPC        IFEQ      *BLANKS
055700950524     C     V1CCPC        OREQ      *ZEROS
055800960904     C     WBOL          IFEQ      'S'
055900960904     C     WSPU          OREQ      'S'
056000071214     C     Worm          orEQ      'S'
056100960904     C                   SETON                                        15
056200960904     C                   ENDIF
056300950524   X2C                   ELSE
056400950525     C                   MOVE(P)   WPDR          WPADR
056500950524    3C     WPADR         IFEQ      DSTPDR
056600960904     C     WBOL          IFEQ      'S'
056700960904     C     WSPU          OREQ      'S'
056800071214     C     Worm          orEQ      'S'
056900960904     C                   SETON                                        15
057000960904     C                   ENDIF
057100950524   X3C                   ELSE
057200950524     C* PADRONCINO DIVERSO DA QUELLO IN DISTINTA: ENTER PER FORZARE
057300960904    4C     WBOL          IFEQ      ' '
057400071214     C     Worm          andeq     ' '
057500950524    5C     WUNO          IFEQ      ' '
057600950524     C                   MOVEL     MSG(6)        V1CMSG
057700950524     C                   SETON                                        4128
057800950524     C                   MOVEL     '1'           WUNO              1
057900060929     C                   leave
058000950524    5C                   ENDIF
058100950524   X4C                   ELSE
058200950524     C                   MOVEL     MSG(7)        V1CMSG
058300950524     C                   SETON                                        4128
058400060929     C                   leave
058500950524    4C                   ENDIF
058600950524    3C                   ENDIF
058700950524    2C                   ENDIF
058800060929     C* SE E' STATO INSERITO IL NUMERO DISTINTA DEVO INIBIRE
058900061002     C* IL CMD15-INSERIMENTO MULTIPLO
059000950526     C     *INKP         IFEQ      *ON
059100950524     C                   MOVEL     MSG(12)       V1CMSG
059200950524     C                   SETON                                        28
059300060929     C                   leave
059400950524    2C                   ENDIF
059500060929     C* 02 ON  - INSERITO la DISTINTA IN PRIMA VIDEATA QUINDI
059600061002     C* SONO IN MANUTENZIONE
059700950525     C                   SETON                                        02
059800950523   X1C                   ELSE
059900950523     C* NUMERO DISTINTA = 0
060000950524    2C     V1CCPC        IFEQ      *BLANKS
060100950523     C     V1CCPC        OREQ      *ZEROS
060200061002     C* SE SI VUOLE INSERIRE UNA DISTINTA OCCORRE IMMETTERE IL
060300061002     C* CODICE PADRONCINO
060400950526    3C     *INKP         IFEQ      *OFF
060500950524     C                   MOVEL     MSG(8)        V1CMSG
060600950523     C                   SETON                                        4128
060700060929     C                   leave
060800950526    3C                   ENDIF
060900950524     C*
061000950524   X2C                   ELSE
061100950524     C* SE PREMUTO CMD15-INSERIMENTO MULTIPLO IL COD.PADRONCINO DEVE
061200061002     C* RIMANERE VUOTO
061300950524    3C     *INKP         IFEQ      *ON
061400950524     C                   MOVEL     MSG(9)        V1CMSG
061500950524     C                   SETON                                        4128
061600060929     C                   leave
061700950524    3C                   ENDIF
061800020910      * Il p.o. del padroncino deve essere uguale al p.o. in gestione
061900020910     c                   If        v1cfpd <> v1cfgs
062000020910     c                   Eval      V1cmsg = Msg(11)
062100020910     c                   Eval      *In41 = *On
062200020910     c                   Eval      *In28 = *On
062300060929     c                   leave
062400020910     c                   EndIf
062500950524    2C                   ENDIF
062600950524     C*
062700950523    1C                   ENDIF
062800060929     c                   enddo
062900061002     C*
063000060929     C                   ENDSR
063100080319     C*-----------------------------------------------------*
063200080319     C* controllo fasi distinta per dati scaricati a PDA         *
063300080319     C*-----------------------------------------------------*
063400080319     C     srfasi        BEGSR
063500080319     c* controllo se i flag per PDA e mat/pom sono modificabili
063600080319     c                   clear                   fasedst           3
063700080319     c                   setoff                                       7273
063800110223     c* se ho la fase CUS (ok da spc) non faccio manutenzionare i flag
063900110223     c* dati a pda e matt/pom sia x con che x orm
064000110223     c*                  eval      dsftrd = 'CUS'
064100110223     C*    KDST4         chain     fidsf02l
064200110223     c*                  if        %found(fidsf02l)
064300110223     c*                  eval      fasedst = dsftrd
064400110223     c*                  seton                                        7273
064500110223     c*                  end
064600110223     c* se ho la fase PDR (preparato x pda ORM)
064700110223     c* non faccio manutenzionare il flag matt/pom
064800110223     c                   eval      dsftrd = 'PDR'
064900110223     C     KDST4         chain     fidsf02l
065000110223     c                   if        %found(fidsf02l)
065100110223     c                   eval      faseorm = dsftrd
065200110223     c                   seton                                        73
065300110223     c                   end
065400080319     c* se ho la fase PDC (preparato x pda con) o PDO x orm
065500080319     c* non faccio manutenzionare il flag matt/pom
065600080319     c                   eval      dsftrd = 'PDC'
065700080326     C     KDST4         chain     fidsf02l
065800080326     c                   if        %found(fidsf02l)
065900080319     c                   eval      fasedst = dsftrd
066000080319     c                   seton                                        73
066100080326     c                   end
066200080319     c*
066300080319     C                   ENDSR
066400060929     C**********************************************************************
066500950524     C*--- GESTIONE SINGOLA VIDEATA DATI FOGLIO DISTINTA -------------*
066600060929     C**********************************************************************
066700950524     C     GESD02        BEGSR
066800950525     C* SE SONO IN IMMISSIONE PRELEVO NUMERATORE FOGLIO DISTINTA
066900950526    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
067000950525     C     *IN03         OREQ      *ON                                          *CMD15
067100950525     C                   MOVEL     CINSER        V1CDES
067200950703     C                   CLEAR                   V2CNFV
067300950525    1C                   ENDIF
067400950524     C* IMPOSTAZIONE DATI VIDEATA
067500950524     C                   EXSR      RIED02
067600950524     C* EMISSIONE FORMATO
067700060929     C                   do        *hival
067800950524     C                   EXFMT     LRF0D02
067900950524     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
068000950524     C                   CLEAR                   V2CMSG
068100950524     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
068200950525     C                   SETOFF                                       284546
068300041008     C                   SETOFF                                       474849
068400160426     C                   SETOFF                                       515255
068500170206     C                   SETOFF                                       44
068600950524     C** CMD12 - RITORNO
068700950525    1C     *INKL         IFEQ      *ON
068800950524     C                   MOVEL     '1'           WGES
068900061006     C                   rolbk
069000950526    2C     *IN01         IFEQ      *ON                                          *IMMISSIONE
069100950525     C     *IN03         OREQ      *ON                                          *CMD15
069200950529     C                   CLEAR                   V1CNFP
069300061011     C                   CLEAR                   V2CNfv
069400950525   X2C                   ELSE
069500070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
069600950525    2C                   ENDIF
069700060929     C                   leave
069800950525    1C                   ENDIF
069900950525     C** CMD16 - ANNULLAMENTO  FOGLIO DISTINTA
070000950525    1C     *INKQ         IFEQ      *ON
070100950524     C                   EXSR      ANNDIS
070200061229     c* aggiorno archivi FNFVV e FNDST
070300061229     c                   if        not *in28
070400061229     c                   eval      d05cmd ='KQ'
070500061229     c                   exsr      sraggold
070600061229     c                   end
070700061229     C   28              iter
070800061002     c                   commit
070900950524     C                   MOVEL     '1'           WGES
071000070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
071100060929     C                   leave
071200950525    1C                   ENDIF
071300950525     C** CMD5  - RIATTIVAZIONE FOGLIO DISTINTA
071400950525    1C     *INKE         IFEQ      *ON
071500950525     C* RIPRISTINO IL RECORD IN FNDST00F
071600061228     c                   z-add     4             wnpg
071700070410     C     KDST          chain     fidst09l
071800070410     c                   if        %found(fidst09l)
071900061228     C                   CLEAR                   DSTATB
072000061228     C                   UPDATE    FiDST000
072100061228     c                   end
072200070717     c* Pianificazione consegne
072300061222     C     KDST1         setll     FiDST31l                           30
072400061222     c                   do        *hival
072500061222     C     KDST1         reade     FiDST31l
072600061222     c                   if        %eof(fidst31l)
072700061222     c                   leave
072800061222     c                   end
072900061222     C                   CLEAR                   DST3ATB
073000061222     C                   UPDATE    FiDST300
073100061222     c                   enddo
073200070717     c* pianificazione ORM
073300070726     c                   if        §OGDDAO <> ' '
073400070717     C     KDST1         setll     FiDST51l                           30
073500070717     c                   do        *hival
073600070717     C     KDST1         reade     FiDST51l
073700070717     c                   if        %eof(fidst51l)
073800070717     c                   leave
073900070717     c                   end
074000070717     C                   CLEAR                   DST5ATB
074100070717     C                   UPDATE    FiDST500
074200070717     c                   enddo
074300070726     c                   end
074400041005     C* RIPRISTINO IL RECORD IN Fiqdt01l
074500070313     c                   if        comdda <> 0
074600041005     C     Kqdt          chain     fiqdt01l                           30
074700041005     C                   clear                   qdtatb
074800041005     C  n30              update    fiqdt000
074900070109     c                   end
075000950525     C                   MOVEL     '1'           WGES
075100070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
075200061229     c* aggiorno archivi FNFVV e FNDST
075300061229     c                   eval      d05cmd ='KE'
075400061229     c                   exsr      sraggold
075500061229     C   28              iter
075600061002     c                   commit
075700060929     C                   leave
075800950525    1C                   ENDIF
075900170207      ***
076000950525     C* CONTROLLI VIDEATA
076100170207      ***
076200170207     C  n28              EXSR      CTRD02
076300170208     C** CMD12 - RITORNO dalla window di disaccreditato
076400170208    1C     *IN44         IFEQ      *ON
076500170208     C                   SETOFF                                       44
076600170208     C                   MOVEL     '1'           WGES
076700170208     C                   rolbk
076800170208    2C     *IN01         IFEQ      *ON                                          *IMMISSIONE
076900170208     C     *IN03         OREQ      *ON                                          *CMD15
077000170208     C                   CLEAR                   V1CNFP
077100170208     C                   CLEAR                   V2CNfv
077200170208   X2C                   ELSE
077300170208     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
077400170208    2C                   ENDIF
077500170208     C                   leave
077600170208    1C                   ENDIF
077700170207      *---
077800060929     C   28              iter
077900950525     C** CMD6  - AGGIORNA
078000070928     c                   if        *inkf or *inkh or *inki
078100061011     c                             or *inkb
078200061002     C                   EXSR      AGGD02
078300060929     C   28              iter
078400070928     c                   if        *inkf or *inkh or *inki
078500070315     c                             or *inkb
078600061229     c* aggiorno archivi FNFVV e FNDST
078700061229     c                   eval      d05cmd ='KF'
078800061229     c                   exsr      sraggold
078900061229     C   28              iter
079000070315     c                   if        not *inkb
079100061006     c                   commit
079200061229     c                   end
079300070315     c                   end
079400061006     c                   end
079500070122     C** CMD2  - altri dati distinata
079600061002     c                   if        *inkb
079700061002     C                   exsr      sraltro
079800070122     c* se nel pgm fidg02r è stato premuto cmd di conferma commit ed
079900061011     c* esco
080000061222     c                   if        d02cmde = 'KF'
080100070322     c                   eval      *inkf = '1'
080200070315     c                   commit
080300070314     c                   else
080400070322     c                   iter
080500061011     c                   end
080600061002     c                   end
080700070928     C** CMD9  - AGGIORNA e chiama gestione e quadratura della singola
080800070928     C**         distinta
080900070928    1C     *INKi         IFEQ      *ON
081000070928     c* emetto window per chiedere la stampante  laser o ad aghi
081100070928     C                   EXSR      wind60
081200070928     c                   if        laser = ' '
081300070928     c                   seton                                        28
081400070928     c                   clear                   V2CMSG
081500070928     C                   iter
081600070928     c                   else
081700070928RDE  C                   CLEAR                   FNLR59ds
081800070928     C                   eval      d59lsr = laser
081900070928RDE  C                   MOVE      V2CFGS        D59fgs
082000070928RDE  C                   MOVE      V2CNFV        D59NDD
082100070928RDE  C                   MOVE      V2CNFV        D59NDA
082200070928RDE  C                   MOVE      V2CDFV        Dtaeur
082300070928RDE  C                   MOVE      dtaeur        Dtaiso
082400070928RDE  C                   MOVE      dtaiso        D59ddt
082500070928RDE  C                   MOVEL(p)  V2dPDR        D59dsf
082600070928RDE  C                   MOVE      V2CPDR        D59pdr
082700070928RDE  C                   MOVEl     V2Ctpm        D59tpm
082800160428RDE  C                   MOVEl     V2Ctur        D59tur1
082900160428RDE  C                   clear                   D59tur2
083000070928RDE  C                   clear                   D59tpd
083100070928RDE  C                   MOVEL     V2CFPP        D59FPP
083200070928     C* STAMPA SOLO LE ANOMALIE
083300070928     C                   MOVEL     'A'           d59STP
083400070928     C* NON STAMPA ORM
083500070928     C                   MOVEL     'N'           d59SOR
083600070928RDE  C                   MOVEL(p)  FNLR59ds      KPJBU
083700070928RDE  C                   CALL      'FNLRF7C'
083800070928RDE  C                   PARM                    KPJBA
083900070928     c                   end
084000070928     c                   end
084100061002     C* SE SONO IN IMMISSIONE(N02) CONTROLLO SE RICHIESTA LA STAMPA
084200070328     C* CMD8 - AGGIORNA E STAMPA ETICHETTA PADRONCINO (IN VARIAZIONE)
084300070328    1C     *INKH         IFEQ      *ON
084400070328     C     *IN02         oreq      *OFF
084500950529     C     *INKF         ANDEQ     *ON
084600950529     C     V1CSTE        ANDEQ     'S'
084700950529     C                   CLEAR                   DSLV25
084800950529     C                   MOVEL     V2CPDR        D25PDR
084900950529     C                   MOVEL     V2DPDR        D25RSC
085000070328     C                   eval      d25nfv = V2CNFV
085100950529     C                   MOVEL     V2CDFV        D25DDC
085200080310     c                   eval      D25fpp = V2Cfpp
085300950529     C                   MOVEL     V2CNOT        D25NOT
085400950908     C                   CALL      PA1PSD
085500950529     C                   PARM                    DSLV25
085600950525    1C                   ENDIF
085700070928     c                   if        *inkf or *inkh or *inki
085800070328     c                   leave
085900070328     c                   end
086000950525     C*
086100061002     c                   enddo
086200950525     C*
086300950525     C                   MOVEL     '1'           WGES
086400070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
086500060929     c*
086600060929     C                   ENDSR
086700070928     C*--------------------------------------------------------------*
086800070928     C*  richiesta tipo stampante
086900070928     C*--------------------------------------------------------------*
087000070928     C     wind60        BEGSR
087100070928     C*
087200070928     c                   clear                   laser             1
087300070928     c                   call      'FNLRF6R1'
087400070928     c                   parm                    laser
087500070928     C*
087600070928     C                   ENDSR
087700060929     C**********************************************************************
087800061002     C*--- PRELEVO NUMERATORE DISTINTA CONSEGNA ---------------*
087900060929     C**********************************************************************
088000950524     C     CRTNFV        BEGSR
088100950703     C                   SETOFF                                       90
088200070122      * se sono in inserimento però ho già un numero di distinta impostato
088300070122      * Controllo che non esista già un foglio con lo stesso numero
088400070122      * richiamo il pgm passando come tipo foglio 8 così controlla anche l'esistenza
088500070122      * del tipo 8
088600070122    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
088700070122     C     *IN03         OREQ      *ON                                          *CMD15
088800070122     c                   if        v2cnfv <> 0
088900070122     c                   Clear                   Fidg03ds
089000070122     c                   Eval      D3Npg = 8
089100070122     c                   Eval      D3Fgs = V2cfgs
089200070122     c                   Z-Add     v2cnfv        D3Ndt
089300070122     c                   Movel(p)  Fidg03ds      Kpjbu
089400070122     c                   Call      'FIDG03R'
089500070122     c                   Parm                    Kpjba
089600070122     c                   Movel     Kpjbu         Fidg03ds
089700070122     c* se ritorno ='N' vuol dire che non ha trovato il numero già usato o
089800070122     c* come foglio o in una bolla ed utilizzo quello
089900070122     c* altrimenti proseguo e ne prelevo uno
090000070122If  2c                   If        D3Flg = 'N'
090100070122     c                   leavesr
090200070122    2c                   EndIf
090300070122    2c                   EndIf
090400070122    2c                   EndIf
090500061002     c*
090600020910Do  1c                   Do        *Hival
090700020910      * Cerco il numero da staccare
090800020910     c                   Clear                   Trul33Ds
090900020910     c                   Eval      I33Cnu = Wcnu
091000020910     c                   Eval      I33Po1 = V2cfgs
091100020910     c                   Eval      I33Num = 1
091200020910     c                   Movel(p)  Trul33Ds      Kpjbu
091300020910     c                   Call      'TRUL33R'
091400020910     c                   Parm                    Kpjba
091500020910     c                   Movel     Kpjbu         Trul33Ds
091600020910
091700020910S   2c                   Select
091800020910      * Numeratore non trovato
091900020910     c                   When      O33err =  01
092000020910     c                   Eval      V2cmsg = Msg(22)
092100020910     c                   Eval      *In28 = *On
092200020910     c                   Eval      *In90 = *On
092300060929     c                   leave
092400020910      * Numeratore allocato
092500020910     c                   When      O33err =  02
092600020910     c                   Eval      V2cmsg = Msg(13)
092700020910     c                   Eval      *In28 = *On
092800020910     c                   Eval      *In90 = *On
092900060929     c                   leave
093000020910
093100020910     c                   When      O33err <> *zeros
093200020910     c                   Eval      V2cmsg = O33Msg
093300020910     c                   Eval      *In28 = *On
093400020910     c                   Eval      *In90 = *On
093500020910    2c                   EndSl
093600020910
093700020910      * Controllo che non esista già un foglio con lo stesso numero
093800020919      * richiamo il pgm passando come tipo foglio 8 così controlla anche l'esistenza
093900020919      * del tipo 8
094000061227     c                   Clear                   Fidg03ds
094100061227     c                   Eval      D3Npg = 8
094200061227     c                   Eval      D3Fgs = V2cfgs
094300061227     c                   Z-Add     O33Nrf        D3Ndt
094400061227     c                   Movel(p)  Fidg03ds      Kpjbu
094500061227     c                   Call      'FIDG03R'
094600061227     c                   Parm                    Kpjba
094700061227     c                   Movel     Kpjbu         Fidg03ds
094800020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
094900020910     c* come foglio o in una bolla
095000061227If  2c                   If        D3Flg <> 'N'
095100061227     c                   Iter
095200061227    2c                   EndIf
095300020910     c                   Z-Add     O33Nrf        V2cnfv
095400020910     c                   Leave
095500020910    1c                   EndDo
095600050426     C*
095700060929     C                   ENDSR
095800060929     C**********************************************************************
095900950524     C*--- IMPOSTAZIONE DATI VIDEATA ---------------------------------*
096000060929     C**********************************************************************
096100950524     C     RIED02        BEGSR
096200950524     C* FILIALE GESTIONE E DECODIFICA
096300950524     C                   MOVEL     V1CFGS        V2CFGS
096400950524     C     V2CFGS        CHAIN     AZORG                              30
096500950525    1C     *IN30         IFEQ      *OFF
096600950524     C                   MOVEL     ORGDES        V2DFGS
096700050519     c* ind 50 per video se p.o. gestisce Parcel+Messaggerie
096800050519     C                   MOVEL     ORGdf0        og150
096900050519     c     §OGPKM        comp      'S'                                    50
097000160426     c     §OGturno      comp      'S'                                    54
097100950525   X1C                   ELSE
097200950524     C                   CLEAR                   V2DFGS
097300160426     c                   setoff                                       5054
097400950525    1C                   ENDIF
097500950526     C****  I M M I S S I O N E  ****
097600950525    1C     *IN01         IFEQ      *ON
097700950525     C                   MOVEL     WPDR          V2CPDR
097800950525     C                   MOVEL     APDRSC        V2DPDR
097900070720     C                   MOVEL     §APDpda       V2cpda
098000080207     C                   MOVEL     §APDtstpda    V2ctstpda
098100110311     c                   move      *zeros        v2cnbn
098200110311     c                   move      apdptp        v2cnbn
098300950525     C                   Z-ADD     UDATE8        V2CDFV                         *DATA DIST.
098400070720     c                   if        v2cpda = ' '
098500070720     c                   eval      v2cpda = 'N'
098600070720     c                   end
098700950607     C* CONTROLLI
098800160427     C                   clear                   vhcdfv
098900160427     C                   clear                   vhrdfv
099000950607     C                   EXSR      CTRDDC
099100061227     C* I CAMPI PRESTAZIONE PADRONCINO (MATT/POM/gg) E PIKING (S/N) LI
099200950525     C*   PROPONGO PRENDENDOLI DALL'ANAGRAFICA
099300061228     C                   MOVEL     'S'           V2CFCA                         *MATTINO
099400950525    2C     HHMM          IFLE      1200
099500950525     C                   MOVEL     'M'           V2CFPP                         *MATTINO
099600950525    3C     APDPKM        IFEQ      ' '
099700950525     C                   MOVEL     'N'           V2CPEP
099800950525   X3C                   ELSE
099900950525     C                   MOVEL     APDPKM        V2CPEP
100000950525    3C                   ENDIF
100100061228    3C     APDcam        IFEQ      ' '
100200061228     C                   MOVEL     'N'           V2Cfca
100300061228   X3C                   ELSE
100400061228     C                   MOVEL     APDcam        V2Cfca
100500061228    3C                   ENDIF
100600950525   X2C                   ELSE
100700950525     C                   MOVEL     'P'           V2CFPP                         *POMERIGGIO
100800950525    3C     APDPKP        IFEQ      ' '
100900950525     C                   MOVEL     'N'           V2CPEP
101000950525   X3C                   ELSE
101100950525     C                   MOVEL     APDPKP        V2CPEP
101200950525    3C                   ENDIF
101300061228    3C     APDcap        IFEQ      ' '
101400061228     C                   MOVEL     'N'           V2Cfca
101500061228   X3C                   ELSE
101600061228     C                   MOVEL     APDcap        V2Cfca
101700061228    3C                   ENDIF
101800950525    2C                   ENDIF
101900050519     c* parcel/messaggerie
102000050519     C                   MOVEL     apdflr        dapdflr                        *MATT/POM
102100150804     c                   if        *in50
102200150804     C                   MOVEL     §apdtpm       v2ctpm                         *MATT/POM
102300150804     c                   else
102400150804     c                   clear                   v2ctpm
102500150804     c                   end
102600050523     c                   if        v2ctpm = ' '
102700050523     c                   eval      v2ctpm = 'P'
102800050523     c                   end
102900160426     c* turno
103000160426     c                   if        *in54
103100160426     C                   MOVEL     §apdturno     v2ctur                         *MATT/POM
103200160426     c                   else
103300160426     c                   clear                   v2ctur
103400160426     c                   end
103500160427     c                   if        v2ctur = ' ' and §apdtpm = ' '
103600160426     c                   eval      v2ctur = '1'
103700160426     c                   end
103800950525     C                   CLEAR                   V2CNOT                         *NOTE
103900950525    1C                   ENDIF
104000950525     C****  M A N U T E N Z I O N E  ****
104100950525    1C     *IN02         IFEQ      *ON                                          *VARIAZIONE
104200950525     C     *IN06         OREQ      *ON                                          *ANNULLATO
104300061002     C* PRENDO I DATI DALLA VIDEATA DELLE SELEZIONI
104400950525    2C     V1CCPC        IFNE      *BLANKS
104500950525     C     V1CCPC        ANDNE     *ZEROS
104600950525     C                   MOVEL     WPDR          V2CPDR
104700950525     C                   MOVEL     APDRSC        V2DPDR
104800070720     C                   MOVEL     §apdpda       V2cpda
104900080207     C                   MOVEL     §apdtstpda    V2ctstpda
105000950525   X2C                   ELSE
105100950525     C* PRENDO I DATI AGGANCIANDO L'ANAGRAFICA PADRONCINI
105200021203     C     kapdd         CHAIN     fiapd000                           32
105300950525    3C     *IN32         IFEQ      *OFF
105400070123     c                   eval      dapdflr = apdflr
105500950525     C                   MOVEL     APDPDR        V2CPDR
105600950525     C                   MOVEL     APDRSC        V2DPDR
105700070123     C                   MOVEL     APDflr        dapdflr
105800070717     C                   MOVEL     §APDpda       V2cpda
105900080207     C                   MOVEL     §APDtstpda    V2ctstpda
106000950525   X3C                   ELSE
106100950525     C                   CLEAR                   V2CPDR
106200950525     C                   CLEAR                   V2DPDR
106300080207     C                   CLEAR                   V2ctstpda
106400070717     C                   CLEAR                   V2cpda
106500950525    3C                   ENDIF
106600950525    2C                   ENDIF
106700160428     c                   setoff                                       10
106800061002     C                   eval      v2cnfv = DSTNFV                              *NUM. DIST.
106900061002     C                   move      dstDFV        dtaiso
107000061002     C                   move      dtaiso        dtaeur
107100061002     C                   MOVE      dtaeur        V2CDFV
107200060929     C* NEL CAMPO CHE SAREBBE IDDEN CI METTO LA DATA distinta
107300950607     C                   MOVEL     V2CDFV        VHCDFV
107400150804     c                   if        *in50
107500150804     C                   MOVEL     dsttpm        v2ctpm                         *MATT/POM
107600150804     c                   else
107700150804     C                   clear                   v2ctpm                         *MATT/POM
107800150804     c                   end
107900050523     c                   if        v2ctpm = ' '
108000050523     c                   eval      v2ctpm = 'P'
108100050523     c                   end
108200160426     c                   if        *in54
108300160426     C                   MOVEL     §dstturno     v2ctur                         *MATT/POM
108400160426     c                   else
108500160426     C                   clear                   v2ctur                         *MATT/POM
108600160426     c                   end
108700160427     c                   if        v2ctur = ' ' and dsttpm = ' '
108800160426     c                   eval      v2ctur = '1'
108900160426     c                   end
109000061228     C                   MOVEL     dstFca        V2CFca                         *MATT/POM
109100070719     C                   MOVEL     dstpda        V2Cpda                         *MATT/POM
109200080207     C                   MOVEL     §dsttstpda    V2Ctstpda                      *MATT/POM
109300061228     C                   MOVEL     dstFPP        V2CFPP                         *MATT/POM
109400060929     C                   MOVEL     dstPEP        V2CPEP                         *PIKING(S/N)
109500041005     C                   MOVEL     qdtnbnaf      V2CNBN                         *NR.BANCALI
109600050120     C                   MOVEL     qdtfnbaf      V2CfNBN                        *NR.BANCALI
109700061228     c                   eval      v2cnot  = %trimr(dstnot)
109800070726     c                   if        v2cpda = ' '
109900070726     c                   eval      v2cpda = 'N'
110000070726     c                   end
110100950525    1C                   ENDIF
110200101022     c* controllo se aut ha o meno il pda
110300080212     c                   clear                   msgv
110400080212     c                   if        *in96
110500101022     c                   exsr      srctrpda
110600080319     C                   ENDIF
110700950525     C*
110800170206      ***----
110900170206      *** Controlla Disaccreditamento Autista
111000170206      **
111100170207     c                   clear                   HPdrForza         7 0
111200170206      ***----
111300170206     C*
111400950524     C                   ENDSR
111500061002     C**********************************************************************
111600950524     C*--- ANNULLAMENTO FOLGIO DISTINTA ------------------------------*
111700061002     C**********************************************************************
111800950524     C     ANNDIS        BEGSR
111900061002     c                   do
112000061002     C* NON E' POSSIBILE ANNULLARE  DISTINTE CONSEGNE CHE HANNO
112100061002     C* BOLLE O SPUNTE SOPRA
112200960904    1C     WSPU          IFEQ      'S'
112300950525     C     WBOL          OREQ      'S'
112400950525     C                   MOVEL     MSG(14)       V2CMSG
112500950525     C                   SETON                                        28
112600061002     c                   leave
112700061002   X1C                   End
112800010314     C* WORM = "S"  --->  ESISTONO O.R.M. IN FASE < 900
112900010314   1AC     WORM          IFEQ      'S'
113000010314     C                   MOVEL     MSG(21)       V2CMSG
113100010314     C                   SETON                                        28
113200061002     c                   leave
113300061002  X1AC                   End
113400061002     C* ANNULLO IL RECORD IN FiDST00F
113500061228     c                   z-add     4             wnpg
113600070410     C     KDST          chain     fidst09l
113700070410     c                   if        %found(fidst09l)
113800061228     C                   MOVEL     'A'           DSTATB
113900061228     C                   UPDATE    FiDST000
114000061228     c                   end
114100061222     C* ANNULLO I RECORD IN FiDST30F
114200061222     C     KDST1         setll     FiDST31l
114300061222     c                   do        *hival
114400061222     C     KDST1         reade     FiDST31l
114500061222     c                   if        %eof(fidst31l)
114600061222     c                   leave
114700061222     c                   end
114800061222     C                   MOVEL     'A'           DST3ATB
114900061222     C                   UPDATE    FiDST300
115000061222     c                   enddo
115100070717     C* ANNULLO I RECORD IN FiDST50F
115200070726     c                   if        §OGDDAO <> ' '
115300070717     C     KDST1         setll     FiDST51l
115400070717     c                   do        *hival
115500070717     C     KDST1         reade     FiDST51l
115600070717     c                   if        %eof(fidst51l)
115700070717     c                   leave
115800070717     c                   end
115900070717     C                   MOVEL     'A'           DST5ATB
116000070717     C                   UPDATE    FiDST500
116100070717     c                   enddo
116200070726     c                   end
116300041005     C* ANNULLO IL RECORD IN Fiqdt01l
116400070313     c                   if        comdda <> 0
116500061002     C     Kqdt          chain     Fiqdt01l
116600061002     c                   if        %found(fiqdt01l)
116700041005     C                   movel     '*'           qdtatb
116800061002     C                   update    fiqdt000
116900070109     c                   end
117000061002    1C                   ENDIF
117100061002     c                   enddo
117200950524     C*
117300950524     C                   ENDSR
117400061002     C**********************************************************************
117500170207     C*--- CONTROLLO AUTISTA ACCREDITATO ----------------------------*
117600170207     C**********************************************************************
117700170207     C     CTRACCR       BEGSR
117800170207     C*
117900170207      *** Controlla Disaccreditamento Autista
118000170207    2C     V2Cpdr        IFNE      *zeros
118100170207    2C     V2Cpdr        andne     HPdrForza
118200170207     c                   clear                   fnlr50wds
118300170207     C                   eval      savpjbu = kpjbu
118400170207     c                   z-add     v2cpdr        §50autista
118500170207     c                   z-add     wdfv          §50datadis
118600170207     c                   eval      §50WINDOW  = 'F'
118700170207     C                   eval         kpjbu = fnlr50wds
118800170207      ***
118900170207     C                   call      'FNLR50WR'
119000170207     C                   parm                    kpjba
119100170207     C                   eval      fnlr50wds = kpjbu
119200170207     C                   eval      kpjbu = savpjbu
119300170207      ***  Se Forzato
119400170207     c                   IF        §50esito ='1'
119500170207     c                   IF        §50funz  ='F07'
119600170207     c                   eval      HPdrForza = V2Cpdr
119700170207     c                   END
119800170207     c                   IF        §50funz  ='F12'
119900170208     c                   eval      HPdrForza = *all'9'
120000170207     C                   MOVEL     MSG(30)       V2CMSG
120100170208     C                   SETON                                        44
120200170207     c                   END
120300170207    1C                   ENDIF
120400170207      ***
120500170207    1C                   ENDIF
120600170207      ***
120700170207     C                   ENDSR
120800170207     C**********************************************************************
120900950607     C*--- CONTROLLO DATA DISTINTA ----------------------------------*
121000061002     C**********************************************************************
121100950607     C     CTRDDC        BEGSR
121200061002     c                   do
121300950607     C* DATA DISTINTA OBBLIGATORIA
121400950607     C                   Z-ADD     V2CDFV        G02DAT
121500950607     C                   MOVEL     *BLANK        G02ERR
121600950607     C                   CALL      'XSRDA8'
121700950607     C                   PARM                    WLBDAT
121800950607    1C     G02ERR        IFEQ      '1'
121900950607     C                   MOVEL     MSG(15)       V2CMSG
122000950607     C                   SETON                                        4528
122100061002     C                   leave
122200950607    1C                   ENDIF
122300950607     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6 (ANCHE NEL SUBFILE)
122400061002     C                   Z-ADD     G02INV        WDFV
122500950607     C                   Z-ADD     G02DAT        V2CDFV
122600950607     C   03              Z-ADD     G02DAT        VSCDFV
122700950607     C* LA DATA NON PUO' ESSERE: - INFERIORE A UDATE
122800950607     C*                          - SUPERIORE A UDATE DI 3 GIORNI
122900950613    0C     *IN15         IFEQ      *OFF
123000950613     C     *IN06         ANDEQ     *OFF
123100950613     C*
123200950607    1C     WDFV          IFLT      DATEU8
123300950607     C                   MOVEL     MSG(16)       V2CMSG
123400950607     C                   SETON                                        4528
123500061002     C                   leave
123600950607    1C                   ENDIF
123700020419      *controllo range fra le date di oggi e quella immessa
123800020419     c                   eval      dadata = dateu8
123900020419     c                   eval      adata  = wdfv
124000020419     c                   CALL      'XSRLAV8'
124100020419     c                   PARM                    Wdata8
124200030917     c                   if        giolav = 0
124300030917     c                   clear                   giorni
124400030917     c                   move      dateu8        wdtiso1
124500030917     c                   move      wdfv          wdtiso2
124600030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
124700030917     c                   if        giorni > 30
124800030917     c                   SETON                                        4528
124900030917     c                   MOVEL     msg(16)       v2cmsg
125000061002     C                   leave
125100030917     c                   endif
125200030917     c                   endif
125300020419     c                   if        giolav > §dgddc
125400020419     c                   SETON                                        4528
125500020419     c                   MOVEL     msg(16)       v2cmsg
125600061002     C                   leave
125700020419     c                   end
125800950607     C* CONTROLLO CHE NON ESISTANO CONTEGGI GIA' CONFERMATI NELLA
125900061002     C* DATA IMMESSA
126000011121     C     KFTT          SETLL     FIFTT02L                               32
126100011121     C  N32KFTT          SETLL     FIFST000                               32
126200950607    1C     *IN32         IFEQ      *ON
126300950607     C                   MOVEL     MSG(17)       V2CMSG
126400950607     C                   SETON                                        4528
126500061002     C                   leave
126600950607    1C                   ENDIF
126700950607     C*
126800950607     C* SE LA DATA E' STATA MODIFICATA CONTROLLO SE ESISTE GIA'
126900061002     C* UNA DISTINTA APERTA PER QUEL PADRONCINO
127000950613    1C     VHCDFV        IFNE      V2CDFV
127100950607     C                   Z-ADD     WDFV          WDFV2
127200950607     C                   Z-ADD     V2CDFV        VHCDFV
127300061002     C                   EXSR      LEGdst
127400061011     c                   end
127500950607     C     V2CPDR        LOOKUP    PDR                                    10
127600950607     C   10              MOVEL     'DS'          VSCDSN
127700950607     C  N10              CLEAR                   VSCDSN
127800061002     c*
127900061011   X1C*                  ELSE
128000061011    2C*    VSCDSN        IFEQ      'DS'
128100061011     C*                  SETON                                        10
128200061011     C*                  ELSE
128300061011     C*                  SETOFF                                       10
128400061011    2C*                  ENDIF
128500061011    1C*                  ENDIF
128600950613    0C                   ENDIF
128700950607     C*
128800061002     c                   enddo
128900061002     C*
129000061002     C                   ENDSR
129100061002     C**********************************************************************
129200950525     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
129300061002     C**********************************************************************
129400950525     C     CTRD02        BEGSR
129500061002     c                   do
129600170208     C* CONTROLLO ACCREDITAMENTO.
129700170208     C                   EXSR      CTRACCR
129800061002     C* DATA DISTINTA
129900950613     C                   EXSR      CTRDDC
130000061002     C   28              leave
130100170206      ***
130200061002     C* NUMERO BOLLE E NUMERO BANCALI
130300950529     C* CONTROLLO CHE NON VENGANO INSERITI VALORI NEGATIVI
130400950529     C                   MLLZO     1             V2CNBN
130500110329     c*                  if        v2cnbn > 0
130600050120     c                   move      'S'           v2cfnbn
130700110329     c*                  endif
130800061227     C* PRESTAZIONE PADRONCINO (MATTINO/POMERIGGIO/giornata)
130900070111    1C     V2CFPP        IFEQ      ' '
131000070111     C                   MOVEL     MSG(19)       V2CMSG
131100070111     C                   SETON                                        4728
131200070111     C                   leave
131300070111    1C                   ENDIF
131400061002     C* PIKING (SI/NO)
131500950526    1C     V2CPEP        IFEQ      ' '
131600950526     C                   MOVEL     MSG(19)       V2CMSG
131700950526     C                   SETON                                        4828
131800950526    1C                   ENDIF
131900070108     C* carico (SI/NO)
132000070108    1C     V2Cfca        IFEQ      ' '
132100070108     C                   MOVEL     MSG(19)       V2CMSG
132200070108     C                   SETON                                        4828
132300070108    1C                   ENDIF
132400080212     c* controllo flag dati per PDA se attivo
132500080212     c                   if        *in96
132600080208     C                   if        (v2cpda = 'O' and
132700080208     c                             §OGPDAORM = ' ') or
132800080208     C                             (v2cpda = 'C' and
132900080208     c                             §OGPDACON = ' ') or
133000080208     C                             (v2cpda = 'E' and
133100080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
133200080208     C                   MOVEL     MSG(26)       V2CMSG
133300080208     c                   select
133400080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
133500080208     c                   eval      v2cmsg = %trim(v2cmsg)+' CONSEGNE'
133600080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
133700080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' ORM'
133800080208     c                   endsl
133900080208     C                   SETON                                        5128
134000080208     C                   leave
134100080208     C                   ENDIF
134200080319     c* se distinta già preparata per pda non posso cambiare il flag x con.
134300080319     c* se ho la fase PDC
134400081110     c                   if        (dstpda = 'E' or dstpda ='C') and
134500081110     c                             (v2cpda ='N' or v2cpda='O') and
134600080319     c                             fasedst ='PDC'
134700080319     c                   seton                                        5128
134800080319     C                   MOVEL     msg(28)       V2CMSG
134900080319     c                   end
135000080319     c* se distinta già preparata per pda non posso cambiare il flag x orm
135100080319     c* se ho il FIPCO00T
135200081110     c                   if        (dstpda = 'E' or dstpda ='O') and
135300081110     c                             (v2cpda = 'N' or v2cpda ='C') and
135400080320     c                              faseorm ='PDR'
135500080319     c                   seton                                        5128
135600080319     C                   MOVEL     msg(28)       V2CMSG
135700080319     c                   end
135800080208     c* controllo flag test per PDA
135900080208     C                   if        (v2ctstpda = 'O' and
136000080208     c                             §OGPDAORM = ' ') or
136100080208     C                             (v2ctstpda = 'C' and
136200080208     c                             §OGPDACON = ' ') or
136300080208     C                             (v2ctstpda = 'E' and
136400080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
136500080208     C                   MOVEL     MSG(26)       V2CMSG
136600080208     c                   select
136700080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
136800080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' CONSEGNE'
136900080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
137000080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' ORM'
137100080208     c                   endsl
137200080208     C                   SETON                                        5228
137300080208     C                   leave
137400080208     C                   ENDIF
137500080215     c* controllo la congruenza dei valori impostati nel test x pda e
137600080215     c* e nei dati x pda
137700080215     C                   if        v2ctstpda <>' '
137800080215     C                   if        (v2cpda = 'O' and
137900080215     C                             v2ctstpda <>'O') or
138000080215     C                             (v2cpda = 'C' and
138100080215     C                             v2ctstpda <>'C') or
138200080215     C                             v2cpda = 'N'
138300080208     C                   MOVEL     MSG(27)       V2CMSG
138400080208     C                   SETON                                        5228
138500080208     C                   leave
138600080208     C                   ENDIF
138700110223     c* si può inserire il flag di test C/E solo se non si sono ricevuti
138800110223     c* esiti consegne (se già inseriti ma non ancora ricevuti, paziena !!!)
138900110223     c                   if        §dsttstpda = ' ' and
139000110223     c                             v2ctstpda <> 'O'
139100110223     c     kpct          setll     fipct02l                               52
139200110223     c                   if        *in52
139300110223     C                   MOVEL     MSG(28)       V2CMSG
139400110223     C                   SETON                                        28
139500110223     C                   leave
139600110223     c                   end
139700110223     c                   end
139800110224     c* se si era in test con Entrambi e si vuole passare in buona con uno
139900110224     c* dei due, non si può fare
140000110530     c*                  if        §dsttstpda = 'E' and
140100110530     c*                            v2ctstpda <> 'E'
140200110530     C*                  MOVEL     MSG(27)       V2CMSG
140300110530     C*                  SETON                                        2852
140400110530     C*                  leave
140500110530     c*                  end
140600110223     c* non è possibile eliminare il flag di test
140700110223     c                   else
140800110223     c                   if        §dsttstpda <> ' ' and
140900110223     c                             (faseorm ='PDR' or fasedst = 'PDC')
141000110223     C                   MOVEL     MSG(28)       V2CMSG
141100110223     C                   SETON                                        5228
141200110223     C                   leave
141300110223     c                   end
141400080215     C                   ENDIF
141500101022     c* controllo se aut ha o no il pda
141600101022     c                   exsr      srctrpda
141700080212     C                   ENDIF
141800160426     c* controllo turno solo per parcel
141900160427     C                   if        v2ctpm = 'M' and v2ctur <> ' '
142000160503     c                             and *in54
142100160426     C                   MOVEL     MSG(29)       V2CMSG
142200160426     C                   SETON                                        5528
142300160426     C                   leave
142400160426     C                   ENDIF
142500160429     C                   if        v2ctpm = 'P' and v2ctur = ' '
142600160503     c                             and *in54
142700160429     C                   MOVEL     MSG(19)       V2CMSG
142800160429     C                   SETON                                        5528
142900160429     C                   leave
143000160429     C                   ENDIF
143100950526     C*
143200170208     C*
143300061002     c                   enddo
143400061002     C*
143500061002     C                   ENDSR
143600101022     c**********************************************************************
143700101022     c     srctrpda      begsr
143800101022     c**********************************************************************
143900101022     c* se procedura attiva e aut. in test avviso
144000101022     c                   clear                   msgv
144100101022     C                   if        (v2ctstpda ='E' and
144200101022     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
144300101022     C                             or (v2ctstpda ='C' and
144400101022     C                             §OGPDACON='S')
144500101022     C                             or (v2ctstpda ='O' and
144600101022     C                             §OGPDAORM='S')
144700101022     C                   eval      msgv= 'ATT. gestione PDA attivata'
144800101022     C                   ENDIF
144900101022     c* se aut. ha dati per pda differenti da quelli della distinta avviso
145000101022     c                   clear                   msgv1
145100101022     C*                  if        (v2ctstpda =' ' and
145200101022     C*                            v2ctstpda <> §apdtstpda) or
145300101022     C                   if        (v2cpda = 'N' and
145400101022     C                             v2cpda <> §apdpda)
145500101022     C                   eval      msgv1= 'ATT. Aut. con PDA'
145600101022     c                   end
145700101022     C*                  if        (v2ctstpda <>' ' and
145800101022     C*                            §apdtstpda =' ') or
145900101022     C                   if        (v2cpda <> 'N' and
146000101022     C                             §apdpda = 'N')
146100101022     C                   eval      msgv1= 'ATT. Aut. senza PDA'
146200101022     c                   end
146300101022     c*
146400101022     C                   ENDSR
146500061002     C**********************************************************************
146600950525     C*--- AGGIORNAMENTO ---------------------------------------------*
146700061002     C**********************************************************************
146800950525     C     AGGD02        BEGSR
146900061227     c                   move      wdfv          dtaiso
147000061227     c                   move      dtaiso        comiso            8 0
147100061222     c                   do
147200950703     C* SE SONO IN IMMISSIONE PRELEVO NUMERATORE FOGLIO DISTINTA
147300070108    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
147400070108     C     *IN03         OREQ      *ON                                          *CMD15
147500070122     C                   EXSR      CRTNFV
147600070122     c                   eval      wnfv = v2cnfv
147700070717     c*
147800070717     c                   clear                   fiapd2ds
147900070515     c* se inserimento multiplo non propone i giri li prende dall'anagrafica
148000070720     c                   if        *inkf
148100070515     c     kapd21        setgt     fiapd22l
148200070515     c     kapd2         readpe    fiapd22l
148300070717     c                   if        (not %eof(fiapd22l) and apd2atb = ' ' and
148400070717     c                             (comiso<apd2dde or comiso>apd2dscad)) or
148500070717     c                             %eof(fiapd22l)
148600070717     c                   clear                   fiapd2ds
148700070717     c                   end
148800070717     c* se passa dalla singola videata propone i giri dell'anagrafica
148900070515     c                   else
149000070515     c                   exsr      sraltro
149100070515     c                   eval      *inkb = '0'
149200070720     c                   if        d02cmde ='KF'
149300070720     c                   eval      *inkf = '1'
149400070720     c                   else
149500070720     c                   seton                                        28
149600070720     C                   MOVEL     MSG(25)       V2CMSG
149700070720     c                   leave
149800070720     c                   end
149900070720     c                   end
150000061011    1C                   ENDIF
150100061002     C* F I D S T 0 0 F
150200061002     c                   clear                   fidstds
150300070410     C     KDST          CHAIN     fidst09l                           3039
150400061228     C* 39 ON - RECORD ALLOCATO
150500061228     C     *IN39         IFEQ      *ON
150600061228     C                   SETON                                        28
150700061228     C                   MOVEL     MSG(20)       V2CMSG
150800061228     C                   leave
150900061228     C                   ENDIF
151000070717     c*allineao i giri se variata la data distinta
151100070410     c                   if        %found(fidst09l) and wdfv <> dstdfv
151200070717     c* consegne
151300061228     c     kdst1         setll     fidst31l
151400061228     c                   do        *hival
151500061228     c     kdst1         reade     fidst31l
151600061228     c                   if        %eof(fidst31l)
151700061228     c                   leave
151800061228     c                   end
151900061228     C                   MOVEL     wdfv          dst3DFV
152000061228     c                   update    fidst300
152100061228     c                   enddo
152200070717     c* ORM
152300070726     c                   if        §OGDDAO <> ' '
152400070717     c     kdst1         setll     fidst51l
152500070717     c                   do        *hival
152600070717     c     kdst1         reade     fidst51l
152700070717     c                   if        %eof(fidst51l)
152800070717     c                   leave
152900070717     c                   end
153000070717     C                   MOVEL     wdfv          dst5DFV
153100070717     c                   update    fidst500
153200070717     c                   enddo
153300061228     c                   end
153400070726     c                   end
153500061228     c*
153600950525     C                   CLEAR                   DSTATB
153700061002     C                   MOVEL     V2CFGS        dstFGS
153800950525     C                   MOVEL     V2CPDR        DSTPDR
153900070122     C                   MOVEL     'A'           DSTtipa
154000061222     C                   MOVEL     wnpg          DSTnpg
154100061228     c                   eval      dstnot  = %trimr(v2cnot)
154200061006     C                   eval      DSTNfv = v2cnfv
154300061006     C                   MOVEL     wdfv          dstDFV
154400061228     C                   MOVEL     V2CFca        dstFca
154500070717     C                   MOVEL     V2Cpda        dstpda
154600080207     C                   MOVEL     V2Ctstpda     §dsttstpda
154700061228     C                   MOVEL     V2CFPP        dstFPP
154800061002     C                   MOVEL     V2CPEP        dstpEP
154900061002     c                   movel     v2ctpm        dsttpm
155000050523     c                   if        v2ctpm = 'P'
155100061002     c                   clear                   dsttpm
155200050523     c                   end
155300160426     c                   movel     v2ctur        §dstturno
155400160426     c                   if        v2ctur = '1'
155500160426     c                   clear                   §dstturno
155600160426     c                   end
155700080207     c                   movel     ddstflr       dstflr
155800070410     c                   if        %found(fidst09l)
155900061002     C                   update    Fidst000
156000061228     c                   else
156100061228     c* INSERIMENTO
156200070717     c* consegne
156300061002     c                   movea     tc1           tc1d
156400061002     c                   movea     ts1           ts1d
156500070123     c                   if        apd2ftc = ' '
156600070123     c                   eval      dstftc = 'E'
156700070123     c                   else
156800061227     c                   eval      dstftc = apd2ftc
156900070123     c                   end
157000070123     c                   if        apd2fts = ' '
157100070123     c                   eval      dstfts = 'E'
157200070123     c                   else
157300070123     c                   eval      dstfts = apd2fts
157400070123     c                   end
157500061227     c                   eval      dstvlu = apd2vlu
157600061222     c                   eval      dstpkl = apd2pkl
157700070222     c                   eval      dstnbl = apd2nbl
157800070122     c                   eval      dstnftl= apd2nftl
157900061002     c                   eval      dsttol = apd2tol
158000070717     c                   if        apd2inc = ' '
158100070123     c                   eval      dstinc = 'S'
158200070123     c                   else
158300070123     c                   eval      dstinc = apd2inc
158400070123     c                   end
158500070123     c                   eval      DSTPKLA=0
158600070123     c                   eval      DSTVLUA=0
158700070123     c                   eval      DSTSTPA=0
158800070222     c                   eval      DSTnblA=0
158900070726     c* ORM attivita
159000070726     c                   if        §OGDDAO <> ' '
159100070717     c                   eval      dstrvlu = apd2rvlu
159200070717     c                   eval      dstrpkl = apd2rpkl
159300070717     c                   eval      dstrnbl = apd2rnbl
159400070717     c                   eval      dstrnftl= apd2rnftl
159500070717     c                   eval      dstrtol = apd2rtol
159600070717     c                   if        apd2rinc = ' '
159700070717     c                   eval      dstrinc = 'S'
159800070717     c                   else
159900070717     c                   eval      dstrinc = apd2rinc
160000070717     c                   end
160100070717     c                   if        apd2rspi = ' '
160200070717     C*???
160300070717     c                   eval      dstrspi = 'N'
160400070717     c                   else
160500070717     c                   eval      dstrspi = apd2rspi
160600070717     c                   end
160700070726     c                   else
160800070726     c                   clear                   dstrvlu
160900070726     c                   clear                   dstrpkl
161000070726     c                   clear                   dstrnbl
161100070726     c                   clear                   dstrnftl
161200070726     c                   clear                   dstrtol
161300070726     c                   clear                   dstrinc
161400070726     c                   clear                   dstrspi
161500070726     c                   end
161600070717     c                   eval      DSTrPKLA=0
161700070717     c                   eval      DSTrVLUA=0
161800070717     c                   eval      DSTrSTPA=0
161900070717     c                   eval      DSTrnblA=0
162000070717     c                   eval      DSTrnbaA=0
162100061002     C                   WRITE     Fidst000
162200070717     c* scrivo i giri consegne
162300061222     c                   clear                   fidst300
162400061222     c                   eval      dst3fgs = dstfgs
162500061227     c                   eval      dst3npg = wnpg
162600061222     c                   eval      dst3dfv = dstdfv
162700061222     c                   eval      dst3nfv = dstnfv
162800070122     c     apd2nras      setll     fiapd31l
162900061222     c                   do        *hival
163000070122     c     apd2nras      reade     fiapd31l
163100061222     c                   if        %eof(fiapd31l)
163200061227     c                   leave
163300061227     c                   end
163400070720     c                   if        apd3atb <> ' '
163500070720     c                   iter
163600070720     c                   end
163700070129     c                   eval      dst3fgsg= apd3fgsg
163800070129     c                   eval      dst3cgi = apd3cgi
163900070122     c                   eval      dst3prgg= apd3prgg
164000061222     c                   eval      dst3sql = apd3sql
164100070723     c                   clear                   fidg09ds
164200070723     c                   eval      d09ifgs = apd3fgsg
164300070723     c                   eval      d09icgi = apd3cgi
164400070723     c                   eval      D09ITUG = 'C'
164500070717     c                   exsr      srcgi
164600070717     c                   if        d09oerr <>'1'
164700061222     c                   write     fidst300
164800070717     c                   end
164900061222     c                   enddo
165000070717     c* scrivo i giri ORM
165100070726     c                   if        §OGDDAO <> ' '
165200070717     c                   clear                   fidst500
165300070717     c                   eval      dst5fgs = dstfgs
165400070717     c                   eval      dst5npg = wnpg
165500070717     c                   eval      dst5dfv = dstdfv
165600070717     c                   eval      dst5nfv = dstnfv
165700070717     c     apd2nras      setll     fiapd51l
165800070717     c                   do        *hival
165900070717     c     apd2nras      reade     fiapd51l
166000070717     c                   if        %eof(fiapd51l)
166100070717     c                   leave
166200070717     c                   end
166300070720     c                   if        apd5atb <> ' '
166400070720     c                   iter
166500070720     c                   end
166600070717     c                   eval      dst5fgsg= apd5fgsg
166700070717     c                   eval      dst5cgi = apd5cgi
166800070717     c                   eval      dst5prgg= apd5prgg
166900070717     c                   eval      dst5sql = apd5sql
167000070723     c                   clear                   fidg09ds
167100070723     c                   eval      d09ifgs = apd5fgsg
167200070723     c                   eval      d09icgi = apd5cgi
167300070723     c                   eval      D09ITUG = 'R'
167400070717     c                   exsr      srcgi
167500070717     c                   if        d09oerr <>'1'
167600070717     c                   write     fidst500
167700070717     c                   end
167800070717     c                   enddo
167900070122     c                   end
168000070726     c                   end
168100070122     C* F I Q D T 0 0 F
168200070313     c                   if        comdda <> 0
168300070122     C     Kqdt          chain     fiqdt01l                           27
168400070122     c                   if        not *in27
168500070122     C                   clear                   qdtatb
168600070122     c                   if        v2cnbn <> qdtnbnaf
168700070122     c                   z-add     v2cnbn        qdtnbnaf
168800070122     c                   z-add     hhmm          qdtoraaf
168900070122     c                   z-add     dateu8        qdtdaiaf
169000070122     c                   move      v2cfnbn       qdtfnbaf
169100070122     C                   endif
169200100318     c                   eval      qdtddc = dstdfv
169300140210     C                   z-add     dstpdr        qdtpdr
169400070122     C                   update    fiqdt000
169500070122     c                   else
169600070122      *SCRITTURA testata quadrature distinte orm RIENTRI
169700070122     c                   clear                   fiqdt000
169800070122     C                   z-add     dstfgs        qdtfgs
169900070122     C                   z-add     dstnfv        qdtnfv
170000070122     C                   z-add     dstdfv        qdtddc
170100070122     C                   z-add     dstpdr        qdtpdr
170200110329     c     kapdd         chain     fiapd01l
170300110329     c                   if        %found(fiapd01l) and apdptp <> *blank
170400110329     c                   move      apdptp        v2cnbn
170500110329     c                   endif
170600110329     c*                  if        v2cnbn > 0
170700070122     c                   z-add     v2cnbn        qdtnbnaf
170800070122     c                   z-add     hhmm          qdtoraaf
170900070122     c                   z-add     dateu8        qdtdaiaf
171000110329     c                   move      'S'           qdtfnbaf
171100110329     C*                  endif
171200070122     C                   WRITE     Fiqdt000
171300070122     c                   end
171400070122     c                   end
171500061222     c                   enddo
171600061002     c*
171700061002     C                   ENDSR
171800061002     C**********************************************************************
171900061229     C* aggiorno FNFVV e FNDST
172000061002     C**********************************************************************
172100061229     C     sraggold      BEGSR
172200070313     c                   if        comdda <> 0
172300061229     C                   eval      d05fgs = V1CFGS
172400061229     c                   eval      d05com= 'N'
172500061229     c                   eval      d05dfv= wdfv
172600061229     c                   eval      d05nfv= wnfv
172700061229     c                   clear                   d05esi
172800061229     c                   eval      kpjbu=fidg05ds
172900061229     c                   call      'FIDG05R'
173000061229     c                   parm                    kpjba
173100061229     c                   eval      fidg05ds = kpjbu
173200061229     c                   if        d05esi <> ' '
173300070315     c                   rolbk
173400061229     c                   eval      v2cmsg = msg(25)
173500061229     c                   seton                                        28
173600061229     c                   end
173700070109     c                   end
173800061229     C                   ENDSR
173900061229     C**********************************************************************
174000061229     C*--- GESTIONE VIDEATA SUBFILE ----------------------------------*
174100061229     C**********************************************************************
174200061229     C     GESS03        BEGSR
174300950525     C* PULIZIA SUBFILE
174400950525     C                   SETON                                        21
174500950525     C                   WRITE     LRF0C03
174600950525     C                   SETOFF                                       21
174700950525     C* CARICAMENTO SUBFILE
174800950525     C                   EXSR      RIES03
174900950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
175000950525     C     NRR           IFEQ      0
175100950525     C                   MOVEL     'F'           WGES
175200061002     C                   else
175300950525     C* EMISSIONE FORMATO
175400061002     C                   do        *hival
175500070207     C                   WRITE     LRF0Z04
175600950525     C                   EXFMT     LRF0C03
175700950525     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
175800950525     C                   CLEAR                   V2CMSG
175900950525     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
176000950525     C                   SETOFF                                       284546
176100041008     C                   SETOFF                                       474849
176200160426     C                   SETOFF                                       080955
176300170207     C                   SETOFF                                       44
176400160426      **
176500160426     C* F11 = Cambia ordinamento
176600160426if  1C                   IF        *INKK
176700160426     C                   EXSR      Ordina
176800160426     C                   iter
176900160426e   1C                   ENDIF
177000950607     C** F3 - FINE
177100950607    1C     *INKC         IFEQ      *ON
177200950607     C                   MOVEL     'F'           WGES
177300950525     C                   CLEAR                   V1CNFP                         *DIST.PRECEDENTE
177400061002     C                   leave
177500950525    1C                   ENDIF
177600950607     C** F12 - RITORNO
177700950607    1C     *INKL         IFEQ      *ON
177800950607     C                   MOVEL     '1'           WGES
177900061006     C                   rolbk
178000061002     C                   leave
178100950607    1C                   ENDIF
178200950526     C* 08 ON  - E' STATO PREMUTO IL CMD1-CONFERMA PER DISTINTA
178300950526     C   KA              SETON                                        08
178400950526     C* 09 ON  - E' STATO PREMUTO IL CMD6-CONFERMA GLOBALE
178500950526     C   KF              SETON                                        09
178600950525     C* CONTROLLO DATI SUBFILE
178700950525     C                   EXSR      CTRS03
178800061002     C   28              iter
178900950607     C** F6  - AGGIORNA
179000950525     C   KF
179100950525     COR KA              EXSR      AGGS03
179200950525     C** ENTER - RIEMETTE LA VIDEATA
179300061011     c                   if        (not *in08 and not *in09) or *in28
179400061011     C                   iter
179500061011     c                   else
179600061011     c                   leave
179700061011     c                   end
179800061002     c                   enddo
179900950526     C* RITORNO ALLA VIDEATA INIZIALE (QUELLA DELLE SELEZIONI)
180000950525     C                   MOVEL     '1'           WGES
180100061002     c                   end
180200950525     C     ENDGS3        ENDSR
180300160426      *-------------------------------------------------------------------------
180400160426      * TASTI FUNZIONALI di ordinamento
180500160426      *-------------------------------------------------------------------------
180600160426     C     Combinazioni  BEGSR
180700160426      **    F11
180800160426     c                   if        ordinamento = '1'
180900160426     c                   eval      ordinamento = '2'
181000160426      **
181100160505     c                   elseIF    ordinamento = '2' and *in54
181200160426     c                   eval      ordinamento = '3'
181300160427      **
181400160505     c                   elseIF    ordinamento = '3' or
181500160505     c                             ordinamento = '2'
181600160427     c                   eval      ordinamento = '1'
181700160426     c                   end
181800160427     c                   if        Ordinamento = '1'
181900160427     c                   eval      vcmd = 'F11=Rag.Soc.'
182000160427      **
182100160505     c                   elseif    Ordinamento = '2' and *in54
182200160427     c                   eval      vcmd = 'F11=Turno/Aut.'
182300160427     c**
182400160505     c                   elseif    Ordinamento = '3' or
182500160505     c                             Ordinamento = '2'
182600160427     c                   eval      vcmd = 'F11=Aut.'
182700160427     c                   end
182800160426      *
182900160426     C                   ENDSR
183000160426      *-------------------------------------------------------------------------
183100160426      * Ordina - TASTO FUNZIONE F11 --> Ragione sociale/cod. aut.
183200160426      *-------------------------------------------------------------------------
183300160426     C     Ordina        BEGSR
183400160426      *
183500160426     c                   exsr      COMBINAZIONI
183600160428      * IMPOSTO ENTRATA AD 'A'
183700160428     C                   EVAL      nrr  = 1
183800160426      **
183900160426     C                   CLEAR                   QLGSCB
184000160426     C                   CLEAR                   QLGSCB00
184100160426      *
184200160426     ** 2 campi chiave
184300160428     C                   EVAL      QLGNBRK    = 1
184400160427     c                   if        ordinamento = '3'
184500160428     C                   EVAL      QLGNBRK    = 2
184600160427     c                   end
184700160427     **
184800160428     C                   EVAL      QLGSO      = Ascendente
184900160426
185000160427     c                   if        ordinamento = '1' or
185100160427     c                             ordinamento = '3'
185200160428     ** il codice autista
185300160428     C                   EVAL      QLGSP      = 1
185400160426     C                   EVAL      QLGSS      = %SIZE(vscpdr)
185500160426     C                   EVAL      QLGDT      = Numerico
185600160428     c* se ordino per codice lo metto al primo campo chiave
185700160428     c                   if        ordinamento = '1'
185800160428     C                   EVAL      QLGKL(1)   = QLGSKL
185900160428     c                   else
186000160428     c* se ordino per turno/codice lo metto al secondo campo chiave
186100160428     C                   EVAL      QLGKL(2)   = QLGSKL
186200160428     c                   end
186300160426      *
186400160426     c                   elseIF    ordinamento = '2'
186500160426     ** La ragione sociale
186600160428     C                   EVAL      QLGSP      = 1 + %SIZE(vscpdr)
186700160427     C                   EVAL      QLGSS      = %SIZE(VSDPDR)
186800160426     C                   EVAL      QLGDT      = Carattere
186900160428     C                   EVAL      QLGKL(1)   = QLGSKL
187000160428     c                   end
187100160428     c                   if        ordinamento = '3'
187200160426     ** turno e codice
187300160428     C                   EVAL      QLGSP      = 1 + %SIZE(vscpdr) +
187400160428     c                                          %SIZE(VSDPDR)
187500160428     C                   EVAL      QLGSS      = %SIZE(vsctur)
187600160426     C                   EVAL      QLGDT      = Carattere
187700160428     C                   EVAL      QLGKL(1)   = QLGSKL
187800160426     C                   ENDIF
187900160426
188000160426
188100160426      * Load other sort parameters.
188200160426     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
188300160426     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
188400160426     C                   EVAL      QLGRT     = 8
188500160426     C                   EVAL      QLGOKL    = 80
188600160426     C                   EVAL      QLGLKE    = 16
188700160426     C                   EVAL      QLGLSS    = 290
188800160426
188900160426      * Initialize Sort I/O API fields.
189000160426     C                   EVAL      QLGRL00  = QLGRL
189100160426     C                   EVAL      QLGRC00  = 1
189200160426     C                   CLEAR                   QUSEI
189300160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
189400160426
189500160426      * First step - Initialize the sort routine.
189600160426     C                   CALL      'QLGSORT'
189700160426     C                   PARM                    QLGSCB
189800160426     C                   PARM                    NotUsed
189900160426     C                   PARM                    NotUsed
190000160426     C                   PARM                    SizeList
190100160426     C                   PARM                    ReturnSize
190200160426     C                   PARM                    QUSEC
190300160426
190400160426      * Next step - Write records to I/O routine.
190500160426     C                   EVAL      QLGRT00 = Put
190600160426
190700160426     C                   DO        STMREC1       x                 4 0
190800160426
190900160427     C     x             CHAIN     LRF0S03
191000160426
191100160426     ** Solo le righe con Selected = 'Y' sono riordinate,
191200160426     ** quindi per fare un ordinamento di tutte le righe
191300160426     ** metto 'Y' sempre.
191400160426     C                   EVAL      Selected = 'Y'
191500160426
191600160426     C                   CLEAR                   QUSEI
191700160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
191800160426     C                   CALL      'QLGSRTIO'
191900160426     C                   PARM                    QLGSCB00
192000160426     C                   PARM                    SflRcd
192100160426     C                   PARM                    NotUsed
192200160426     C                   PARM                    SizeList
192300160426     C                   PARM                    NotUsed
192400160426     C                   PARM                    QUSEC
192500160426
192600160426     C                   ENDDO
192700160426
192800160426      * Next step - Signal end of input, clear subfile for reload.
192900160426     C                   EVAL      QLGRT00 = EndPut
193000160426     C                   CLEAR                   QUSEI
193100160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
193200160426
193300160426     C                   CALL      'QLGSRTIO'
193400160426     C                   PARM                    QLGSCB00
193500160426     C                   PARM                    SflRcd
193600160426     C                   PARM                    NotUsed
193700160426     C                   PARM                    SizeList
193800160426     C                   PARM                    NotUsed
193900160426     C                   PARM                    QUSEC
194000160426
194100160427     C                   SETOn                                        21
194200160427     C                   WRITE     LRF0c03
194300160427     C                   SETOff                                       21
194400160426
194500160426      * Final step - Write the records back to the subfile.
194600160426     C                   EVAL      QLGRT00 = Get
194700160426
194800160426     C                   DO        STMREC1       x
194900160426     C                   CLEAR                   QUSEI
195000160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
195100160426     C                   CALL      'QLGSRTIO'
195200160426     C                   PARM                    QLGSCB00
195300160426     C                   PARM                    NotUsed
195400160426     C                   PARM                    SflRcd
195500160426     C                   PARM                    QLGRL00
195600160426     C                   PARM                    NotUsed
195700160426     C                   PARM                    QUSEC
195800160426      *
195900160427     c                   z-add     x             nrr
196000160428     C     VSCDSN        IFEQ      'DS'
196100160428     C                   SETON                                        10
196200160428     C                   ELSE
196300160428     C                   SETOFF                                       10
196400160428     C                   ENDIF
196500160427     C                   WRITE     LRF0S03
196600160426     C                   ENDDO
196700160426      *
196800160426     C                   ENDSR
196900061002     C**********************************************************************
197000950525     C*--- CARICAMENTO SUBFILE ---------------------------------------*
197100061002     C**********************************************************************
197200950525     C     RIES03        BEGSR
197300170206     c                   setoff                                       44
197400950525     C                   Z-ADD     0             NRR               4 0
197500950525     C                   MOVEL(P)  V1CFPD        WPADR
197600950607     C* CARICO IN SCHIERA I PADRONCINI CHE HANNO GIA' 1 DISTINTA APERTA
197700050519     c
197800050519     c                   if        v05dfv = 0
197900950607     C                   Z-ADD     DATEU8        WDFV2
198000050519     c                   else
198100050519     C                   Z-ADD     V05DFV        G02DAT
198200050519     C                   MOVEL     *BLANK        G02ERR
198300050519     C                   CALL      'XSRDA8'
198400050519     C                   PARM                    WLBDAT
198500050519     C                   Z-ADD     G02INV        WDFV2
198600050519     c                   end
198700061002     C                   EXSR      LEGdst
198800061002     C* CARICO I PADRONCINI
198900021203     C     kapdw         SETLL     fiapd000
199000021203     C     apdtip        READe     fiapd000                               32
199100950525     C*
199200950525    1C     *IN32         DOWEQ     *OFF
199300000619     C*CONTROLLO SE INSERITO CODICE PARZIALIZZAZZIONE TIPO PADRONCINO
199400061011     C                   MOVE      *BLANK        PADROK            1
199500050519     c* se scelto messaggerie o parcel
199600050519     c                   movel     apdflr        dapdflr
199700050523     c                   if        v05tpm <> ' '
199800050523     c                   if        (§apdtpm = ' ' and v05tpm <> 'P') or
199900050523     c                             (§apdtpm = 'M' and v05tpm <> 'M')
200000050519     C                   MOVE      'X'           PADROK
200100050519     c                   end
200200050523     c                   end
200300950525     C* ESCLUDO RECORD ANNULLATI
200400950525    2C     APDATB        IFEQ      ' '
200500140804     C* ESCLUDO I CODICI DI autisti occasionali
200600140804     C     APDsnu        ANDEQ     ' '
200700000619      * PADRONCINO CORRISPONDENTE ALLA PARZIALIZZAZIONE
200800000619     C     PADROK        ANDEQ     ' '
200900950525     C* CONTROLLO LA FILIALE DEL CODICE PADRONCINO
201000950525     C                   MOVEL     APDPDR        W0030             3 0
201100950525    3C     W0030         IFEQ      V1CFPD
201200950525     C                   CLEAR                   VSCSCE
201300950525     C                   MOVEL     APDPDR        VSCPDR
201400950525     C                   MOVEL     APDRSC        VSDPDR
201500070720     C                   MOVEL     §apdpda       VScpda
201600080207     C                   MOVEL     §apdtstpda    VSctstpda
201700080208     c* blink se aut. in test e procedura partita
201800080208     c                   if        wdfv2>= dateu8
201900080208     C                   if        (vsctstpda ='E' and
202000080208     C                             (§OGPDAORM ='S' and §OGPDACON='S'))
202100080208     C                             or (vsctstpda ='C' and
202200080208     C                             §OGPDACON='S')
202300080208     C                             or (vsctstpda ='O' and
202400080208     C                             §OGPDAORM='S')
202500080208     C                   seton                                        53
202600080208     c                   else
202700080208     c                   setoff                                       53
202800080208     C                   ENDIF
202900080208     c                   end
203000080208     c*
203100070726     c                   if        vscpda = ' '
203200070726     c                   eval      vscpda = 'N'
203300070726     c                   end
203400050519     c                   if        v05dfv = 0
203500050519     C                   MOVEL     UDATE8        VSCDFV
203600050519     C                   MOVEL     UDATE8        VHCDFV
203700050519     c                   else
203800050519     C                   MOVEL     v05dfv        VSCDFV
203900050519     C                   MOVEL     v05dfv        VHCDFV
204000050519     c                   end
204100950606     C* INDICO SE E' STATA GIA' APERTA IN QUEL GIORNO UNA DISTINTA PER
204200061002     C* QUEL PADRONCINO
204300950607     C     APDPDR        LOOKUP    PDR                                    10
204400950607     C   10              MOVEL     'DS'          VSCDSN
204500950607     C  N10              CLEAR                   VSCDSN
204600950525     C*
204700950525     C* PRESTAZIONE PADRONCINO (MATT/POM) E PIKING (S/N)
204800050719     c* se impostato nella finestra delle parzializzazioni
204900070111     c                   if        v05fpp <> *blanks
205000050719     C                   MOVEL     v05fpp        VSCfpp
205100070111     c                   end
205200070111    4C     HHMM          IFLE      1200
205300050719     c* lo imposto solo se non immesso nella finestra
205400070111     c                   if        vscfpp = *blanks
205500070111     C                   MOVEL     'M'           VSCFPP                         *MATTINO
205600070111     c                   end
205700070111   X4C                   ELSE
205800070111     c                   if        vscfpp = *blanks
205900070111     C                   MOVEL     'P'           VSCFPP                         *POMERIGGIO
206000070111     c                   end
206100070111    4C                   ENDIF
206200050923     c*
206300070111     c                   if        vscfpp = 'M'
206400050923     C     APDPKM        IFEQ      ' '
206500050923     C                   MOVEL     'N'           VSCPEP
206600050923     C                   ELSE
206700050923     C                   MOVEL     APDPKM        VSCPEP
206800050923     C                   ENDIF
206900050923     c                   else
207000050923     C     APDPKP        IFEQ      ' '
207100050923     C                   MOVEL     'N'           VSCPEP
207200050923     C                   ELSE
207300050923     C                   MOVEL     APDPKP        VSCPEP
207400050923     C                   ENDIF
207500050923     C                   ENDIF
207600050519     c*
207700150804     c                   if        *in50
207800150804     C                   MOVEL     §apdtpm       VSCtpm
207900150804     c                   else
208000150804     c                   clear                   vsctpm
208100150804     c                   end
208200150804     c                   if        vsctpm = ' '
208300050523     C                   MOVEL     'P'           VSCtpm
208400050523     c                   end
208500950525     C*
208600160426     c                   if        *in54
208700160426     C                   MOVEL     §apdturno     VSCtur
208800160426     c                   else
208900160426     c                   clear                   vsctur
209000160426     c                   end
209100160427     c                   if        vsctur = ' ' and §apdtpm = ' '
209200160426     C                   MOVEL     '1'           VSCtur
209300160426     c                   end
209400160426     C*
209500170207     c                   clear                   hPdrForza
209600170206     C*
209700950525     C                   ADD       1             NRR
209800950525     C                   WRITE     LRF0S03
209900950525     C*
210000950525   X3C                   ELSE
210100950525     C                   SETON                                        32
210200950525    3C                   ENDIF
210300950525    2C                   ENDIF
210400950525     C*
210500021203     C  N32apdtip        READe     fiapd000                               32
210600950525    1C                   ENDDO
210700160427     C*  Ordina per Codice Cliente
210800160428     c                   eval      Ordinamento   ='3'
210900160427     C                   Z-ADD     nrr           STMREC1
211000160426     c                   exsr      Ordina
211100950525     C*
211200950525     C                   Z-ADD     1             REC
211300950525     C                   ENDSR
211400061002     C**********************************************************************
211500950607     C*--- LETTURA DISTINTE APRTE IN UNA DATA ------------------------*
211600061002     C**********************************************************************
211700061002     C     LEGdst        BEGSR
211800950607     C                   CLEAR                   PDR
211900160427     C                   Z-ADD     1             Xy
212000070410     C     Kdst2         SETLL     fidst08l
212100061002     c                   do        *hival
212200070410     C     KDST2         reade     fidst08l
212300070410     c                   if        %eof(fidst08l)
212400061002     c                   leave
212500061002     c                   end
212600070209     C     dstATB        IFEQ      ' '
212700070209     C     dstFCF        ANDEQ     ' '
212800160427     C                   MOVEL     DSTPDR        PDR(Xy)
212900170207     C                   ADD       1             Xy                5 0
213000070209     c                   end
213100950607     C                   ENDDO
213200950607     C                   ENDSR
213300061002     C**********************************************************************
213400950525     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
213500061002     C**********************************************************************
213600950525     C     CTRS03        BEGSR
213700950526     C                   CLEAR                   WSCE
213800950607     C*
213900950607    0C     *IN09         IFEQ      *OFF
214000950607     C* SE NON HO PREMUTO F6-LEGGO IN READC PER CONTROLLARE SE HO
214100061002     C* MODIFICATO DELLE DATE
214200160426     C                   READC     LRF0S03                                31
214300160426    1C     *IN31         DOWEQ     *OFF
214400950607     C                   Z-ADD     NRR           REC
214500950607     C                   MOVEL     VSCPDR        V2CPDR
214600070720     C                   MOVEL     VScpda        V2cpda
214700080207     C                   MOVEL     VSctstpda     V2ctstpda
214800950607     C                   MOVEL     VSCDFV        V2CDFV
214900950607     C* CONTROLLI
215000950607     C                   EXSR      CTRDDC
215100971014     C  N28              Z-ADD     WDFV          VHRDFV
215200170206     c*----
215300170206      ***
215400170206     c*----
215500160426     c* turno
215600160429     C                   if        vsctpm = 'M' and vsctur <> ' '
215700160426     C                   MOVEL     MSG(29)       V2CMSG
215800160426     C                   SETON                                        5528
215900160426     C                   end
216000160429     C                   if        vsctpm = 'P' and vsctur = ' '
216100160429     C                   MOVEL     MSG(19)       V2CMSG
216200160429     C                   SETON                                        5528
216300160429     C                   end
216400160426     C                   UPDATE    LRF0S03
216500160426     C   28              SETON                                        31
216600160426     C  N31              READC     LRF0S03                                31
216700160426    1C                   ENDDO
216800160426     C   28              GOTO      ENDS03
216900160426    0C                   ENDIF
217000950526     C                   Z-ADD     1             NRR
217100950526     C     NRR           CHAIN     LRF0S03                            31
217200950526    1C     *IN31         DOWEQ     *OFF
217300950526     C* WSCE = "1" --> E' STATO SELEZIONATO ALMENO UN RECORD DEL SFL
217400950526     C     VSCSCE        IFEQ      '1'
217500950526     C                   MOVEL     '1'           WSCE              1
217600950526     C                   SETON                                        31
217700950526     C                   ENDIF
217800950526     C*
217900950526     C                   ADD       1             NRR
218000950526     C  N31NRR           CHAIN     LRF0S03                            31
218100950526    1C                   ENDDO
218200950607     C* LEGGO IL SFL PER CONTROLLARE LA VALIDITA' DEI DATI IMMESSI
218300061002     c*
218400950607     C* SOLO SE E' STATO PREMUTO CMD6-CONFERMA GLOBALE PERCHE'
218500950607     C*   ALTRIMENTI I CONTROLLI LI EFFETTUO PASSANDO DA OGNI VIDEATA
218600950607    0C     *IN09         IFEQ      *ON                                          *CMD6
218700950525     C                   Z-ADD     1             NRR
218800950525     C     NRR           CHAIN     LRF0S03                            31
218900950525    1C     *IN31         DOWEQ     *OFF
219000950529     C                   SETOFF                                       4546
219100041008     C                   SETOFF                                       474849
219200160426     C                   SETOFF                                       515255
219300170206     C                   SETOFF                                       44
219400950526     C* WSCE = "1" --> CONTROLLO SOLO I RECORD DEL SFL SELEZIONATI
219500950526    2C     WSCE          IFEQ      '1'
219600950526     C     VSCSCE        ANDEQ     '1'
219700950526     C* WSCE = " " --> CONTROLLO TUTTI I RECORD DEL SFL
219800950526     C     WSCE          OREQ      ' '
219900950526     C* IMPOSTO CAMPI UTILIZZATI NELLE ROUTINE COMUNI
220000070720     C                   MOVEL     VScpda        V2cpda
220100080207     C                   MOVEL     VSctstpda     V2ctstpda
220200070123     C                   MOVEL     VSCPDR        V2CPDR
220300950525     C                   MOVEL     VSCDFV        V2CDFV
220400950526     C                   MOVEL     VSCFPP        V2CFPP
220500950526     C                   MOVEL     VSCPEP        V2CPEP
220600050519     C                   MOVEL     VSCtpm        V2Ctpm
220700160426     C                   MOVEL     VSCtur        V2Ctur
220800070108     C                   MOVEL     'S'           V2Cfca
220900950526     C* CONTROLLI
221000950525     C                   EXSR      CTRD02
221100170206      **----
221200170207      **-  ATTENZIONE AL DISACCREDITATO FORZATO O NO
221300170207      **  se NON ha Forzato  si memorizza "9999999" nel codice Hidden
221400170207      *    e prosegue senza fermarsi accendendo solo una segnalazione di codice
221500170208     c                   if        *in44
221600170207     c                   setoff                                       28
221700170207     c                   end
221800170207      **----
221900950525     C   28              Z-ADD     NRR           REC
222000971014     C  N28              Z-ADD     WDFV          VHRDFV
222100950525     C*
222200950525     C                   UPDATE    LRF0S03
222300950525     C   28              SETON                                        31
222400950526    2C                   ENDIF
222500950525     C                   ADD       1             NRR
222600950525     C  N31NRR           CHAIN     LRF0S03                            31
222700950525    1C                   ENDDO
222800950607    0C                   ENDIF
222900950607     C*
223000950607     C     ENDS03        ENDSR
223100061002     C**********************************************************************
223200950525     C*--- AGGIORNAMENTO SUBFILE -------------------------------------*
223300061002     C**********************************************************************
223400950525     C     AGGS03        BEGSR
223500061002     c*
223600061003     C                   Z-ADD     1             NRR
223700061003     C     NRR           CHAIN     LRF0S03                            31
223800061003    1C     *IN31         DOWEQ     *OFF
223900950526     C* WSCE = "1" --> AGGIORNO SOLO I RECORD DEL SFL SELEZIONATI
224000950526    2C     WSCE          IFEQ      '1'
224100950526     C     VSCSCE        ANDEQ     '1'
224200950526     C* WSCE = " " --> AGGIORNO TUTTI I RECORD DEL SFL
224300950526     C     WSCE          OREQ      ' '
224400170207      *
224500170207     C*  Se deve essere saltato
224600170207    2C                   IF        hPDRforza <> *ALL'9'
224700170207      *
224800950526     C* IMPOSTO CAMPI UTILIZZATI NELLE ROUTINE COMUNI
224900070720     C                   MOVEL     VScpda        V2cpda
225000080207     C                   MOVEL     VSctstpda     V2ctstpda
225100070123     C                   MOVEL     VSCPDR        V2CPDR
225200950525     C                   MOVEL     VSDPDR        V2DPDR
225300950525     C                   MOVEL     VSCDFV        V2CDFV
225400950525     C                   MOVEL     VSCFPP        V2CFPP
225500950525     C                   MOVEL     VSCPEP        V2CPEP
225600050519     C                   MOVEL     VSCtpm        V2Ctpm
225700160426     C                   MOVEL     VSCtur        V2Ctur
225800070516     C                   MOVEL     'S'           V2Cfca
225900950607     C                   CLEAR                   V2CNOT
226000950607     C     VSCDSN        IFEQ      'DS'
226100950607     C                   SETON                                        10
226200950607     C                   ELSE
226300950607     C                   SETOFF                                       10
226400950607     C                   ENDIF
226500950526     C** CMD1  - CONFERMA PER DISTINTA: PASSA DALLA SINGOLA VIDEATA
226600950526    3C     *IN08         IFEQ      *ON
226700950525     C                   EXSR      GESD02
226800950525     C* RECORD FILE NUMERATORI ALLOCATO: RITORNO IN PRIMA VIDEATA
226900061002     C   39              leave
227000950526    3C                   ENDIF
227100950525     C** CMD6  - AGGIORNA SENZA PASSARE DALLA SINGOLA VIDEATA
227200950526    3C     *IN09         IFEQ      *ON
227300950529     C                   MOVEL     V1CFGS        V2CFGS
227400950703     C* AGGIORNAMENTO
227500971014     C                   Z-ADD     VHRDFV        WDFV
227600950525     C                   EXSR      AGGD02
227700170207     c*
227800061229     c* aggiorno archivi FNFVV e FNDST
227900061229     c                   if        not *in28
228000061229     c                   eval      d05cmd ='KF'
228100061229     c                   exsr      sraggold
228200061229     c                   end
228300070322     c*
228400070322     c                   if        *in28
228500070322     c                   rolbk
228600070322     C                   leave
228700070322     c                   else
228800061229     c                   commit
228900070322     c                   end
229000950529     C                   MOVEL     V2CNFV        V1CNFP                         *DIST.PRECEDENTE
229100950529     C* SE RICHIESTA ESEGUO STAMPA ETICHETTA PADRONCINO
229200950529    4C     V1CSTE        IFEQ      'S'
229300950529     C                   CLEAR                   DSLV25
229400950529     C                   MOVEL     V2CPDR        D25PDR
229500950529     C                   MOVEL     V2DPDR        D25RSC
229600070328     C                   eval      d25nfv = V2CnFV
229700950529     C                   MOVEL     V2CDFV        D25DDC
229800080310     c                   eval      D25fpp = V2Cfpp
229900950529     C                   MOVEL     V2CNOT        D25NOT
230000950908     C                   CALL      PA1PSD
230100950529     C                   PARM                    DSLV25
230200950529    4C                   ENDIF
230300950529    3C                   ENDIF
230400950526    2C                   ENDIF
230500170207     c                   end
230600170207      *---
230700070108     c                   seton                                        10
230800070108     C   10              MOVEL     'DS'          VSCDSN
230900070108     c                   update    lrf0s03
231000061003     C                   ADD       1             NRR
231100061003     C  N31NRR           CHAIN     LRF0S03                            31
231200950525    1C                   ENDDO
231300950525     C*
231400061002     C                   ENDSR
231500050519     C**********************************************************************
231600050519     C*--- GESTIONE VIDEATA DELLE SELEZIONI --------------------------*
231700050519     C**********************************************************************
231800050519     C     GESD05        BEGSR
231900050617     C                   SETOFF                                       45
232000050519     C                   CLEAR                   V05tpm
232100050519     C                   eval      V05dfv = udate8
232200050719    2C     HHMM          IFLE      1200
232300050719     C                   MOVEL     'M'           V05FPP                         *MATTINO
232400050719     c                   else
232500050719     C                   MOVEL     'P'           V05FPP                         *MATTINO
232600050719     c                   end
232700050519     C* EMISSIONE FORMATO
232800050519     C                   do        *hival
232900050519     C                   EXFMT     LRF0D05
233000050617     C                   SETOFF                                       4528
233100050519     C* CONTROLLo data distinta
233200050519     C                   MOVEL     v05dfv        V2CDFV
233300160427     c                   clear                   vhcdfv
233400160427     c                   clear                   vhrdfv
233500050519     C                   EXSR      CTRDdc
233600050617     C                   MOVEL     v2cdfv        V05DFV
233700050519     C   28              iter
233800050519     c                   leave
233900050519     c                   enddo
234000050519     C*
234100050519     C                   ENDSR
234200061002     C**********************************************************************
234300061222     C*  chiamo fidg02r x gestire gli altri dati della distinta
234400061002     C**********************************************************************
234500061002     C     sraltro       BEGSR
234600061002     C*
234700070720     c                   clear                   okagg             1
234800070720     c                   clear                   fidg02ds
234900070122     C* SE SONO IN IMMISSIONE passo comando KJ
235000070122    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
235100070122     C     *IN03         OREQ      *ON                                          *CMD15
235200070122     c                   eval      d02cmd = 'KJ'
235300070913     c* questo flag mi serve per simulare l'inserimento anche se sono in
235400070913     c* modifica xchè ho già inserito il record
235500070913     c                   eval      d02ana = '1'
235600070122     c                   else
235700070122     c                   eval      d02cmd = 'KF'
235800070122     c                   end
235900061222     c                   eval      d02fil = v2cfgs
236000070122     c                   eval      d02tipa= 'A'
236100061222     c                   eval      d02pdr = v2cpdr
236200061222     c                   eval      d02rsc = v2dpdr
236300070320     c                   eval      d02strcmt = 'N'
236400070320     c                   eval      d02opncmt = 'S'
236500070320     c                   eval      d02com = 'N'
236600061222     c                   eval      d02file= 'FIDST00F'
236700061222     c                   move      wnpg          d02npg
236800061222     c                   move      v2cdfv        dtaeur
236900061006     c                   move      dtaeur        dtaiso
237000061222     c                   move      dtaiso        d02dfv
237100061222     c                   eval      d02nfv = v2cnfv
237200061222     c                   eval      d02fpp = v2cfpp
237300061222     c                   eval      d02tpm = v2ctpm
237400070720     c                   eval      d02tp  = 'C'
237500061222     c                   eval      kpjbu = fidg02ds
237600070723     c* faccio 2 giri: uno per consegne uno per ritiri solo se abilitato
237700070726     c                   eval      yy =1
237800070726     c                   if        §OGDDAO <> ' '
237900070726     c                   eval      yy = 2
238000070726     c                   end
238100070726     c                   do        yy            xx                1 0
238200070720     c                   if        xx = 2
238300070720     c                   eval      d02tp  = 'R'
238400070720     c* se inseriti i giri x consegne, x i ritiri entro in aggiornamento
238500070720     c* altrimenti lascio inserimento
238600070913     c                   if        okagg = '1'
238700070913     c                   eval      d02cmd = 'KF'
238800070913     c                   end
238900070720     c                   eval      kpjbu = fidg02ds
239000070720     c                   end
239100061222     c                   call      'FIDG02R'
239200061002     c                   parm                    kpjba
239300061222     c                   movel(p)  kpjbu         fidg02ds
239400070720     c                   if        xx = 1 and
239500070720     c                             d02cmde = 'KF'
239600070720     c                   eval      okagg = '1'
239700070720     c                   end
239800070720     c                   enddo
239900070720     c*
240000070720     c                   if        okagg = '1' and d02cmde <> 'KF'
240100070720     c                   eval      d02cmde = 'KF'
240200070720     c                   end
240300061002     C*
240400061002     C                   ENDSR
240500070717     C**************************************************************************
240600070717     C* controllo e decodifico giro
240700070717     C**************************************************************************
240800070717     C     Srcgi         BEGSR
240900070717     C*
241000070717     c                   eval      D09IOP0 = '001'
241100070717     c                   move      dstdfv        d09idat
241200070717     c                   eval      kpjbu = fidg09ds
241300070717     c                   call      'FIDG09R'
241400070717     c                   parm                    kpjba
241500070717     c                   eval      fidg09ds = kpjbu
241600070717     C*
241700070717     C                   ENDSR
241800061002     C**********************************************************************
241900061002     C*--- INZSR INIZIALE --------------------------------------------*
242000061002     C**********************************************************************
242100061002     C     *INZSR        BEGSR
242200061002     C*
242300061002     C     *ENTRY        PLIST
242400061002     C                   PARM                    KPJBA
242500061002     C                   MOVEL     KPJBU         PARAM1
242600061002      * Reperisco dati job
242700061002     c                   exsr      DatiJob
242800061002
242900061002     c     Karb78        Klist
243000061002     c                   Kfld                    Karbifp
243100061002     c                   Kfld                    Karbndc
243200070223     c     Karg          Klist
243300070223     c                   Kfld                    Kargfgs
243400070223     c                   Kfld                    Kargndc
243500061002     C***
243600061002     c     Kapdw         Klist
243700061002     c                   Kfld                    apdtip
243800061002     c                   Kfld                    wpadr
243900061002     c     Kapdd         Klist
244000061002     c                   Kfld                    apdtip
244100061002     c                   Kfld                    dstpdr
244200061002     c                   movel     'A'           apdtip
244300061222     c     Kapd21        Klist
244400061222     c                   Kfld                    apdtip
244500061227     c                   Kfld                    v2cpdr
244600061227     c                   Kfld                    v2cfpp
244700061222     c                   Kfld                    comiso
244800061222     c     Kapd2         Klist
244900061222     c                   Kfld                    apdtip
245000061227     c                   Kfld                    v2cpdr
245100061227     c                   Kfld                    v2cfpp
245200110223     c     Kpct          Klist
245300110223     c                   Kfld                    dstFGS
245400110223     c                   Kfld                    dstNFV
245500110223     c                   Kfld                    dstpdr
245600110324     c                   Kfld                    wtrd              3
245700110324     c                   eval       wtrd  = 'CET'
245800061002     C***
245900061002     C     KTBE          KLIST
246000061002     C                   KFLD                    KCOD1
246100061002     C                   KFLD                    KKE1
246200061002     C*  tabelle dgd x reperimento limiti chiusura spedizioni
246300061002     C                   MOVEL     'DGD'         KCOD1
246400061002     C                   MOVEL     *BLANKS       KKE1
246500061002     C                   MOVEL     '1'           KKE1
246600061002     C     KTBE          CHAIN     TNTBE01L                           31
246700061002     C     *IN31         IFEQ      '0'
246800061002     C                   MOVEL     TBEUNI        DDGD
246900061002     C                   END
247000061002     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
247100061002     C***
247200061002     C                   TIME                    W0140            14 0
247300061002     C                   MOVE      W0140         UDATE8            8 0
247400061002     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
247500061002     C*
247600061002     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
247700061002     C                   MOVEL     *BLANK        G02ERR
247800061002     C                   CALL      'XSRDA8'
247900061002     C                   PARM                    WLBDAT
248000061002     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
248100070424     c* se ricevuto p.o. imposto e proteggo
248200070424     c                   if        pa1fgs <> ' '
248300070424     C                   MOVEL     pa1fgs        V1CFGS                         *FIL.GESTIONE
248400070424     C                   MOVEL     pa1fgs        V1CFPD                         *FIL.PADRONCINO
248500070424     C                   SETON                                        05
248600070424     c                   else
248700061003     C                   MOVEL     dutpou        V1CFGS                         *FIL.GESTIONE
248800061003     C                   MOVEL     dutpou        V1CFPD                         *FIL.PADRONCINO
248900061003     c                   if        dutlpo = '2'
249000061002     C                   SETON                                        05
249100061002    1C                   ENDIF
249200070424    1C                   ENDIF
249300061002     C*
249400061002     C* WGES = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
249500061002     C                   MOVEL     '1'           WGES              1
249600061002     C*
249700061227     C                   MOVEL     'FIDG01R'     V1CPGM
249800061003     C                   Z-ADD     4             WNPG              1 0
249900061002     C                   Z-ADD     12            WCNU
250000061002     C                   MOVEL     'C'           WFVL
250100061002     C                   CLEAR                   WTSR
250200061002     C                   CLEAR                   V1CNFP
250300061002     C                   MOVEL     'S'           V1CSTE
250400080411     C*****              MOVEL     'N'           V1CSNB
250500061002     C*
250600061002     C***
250700061002     C* DEFINIZIONE CAMPI
250800061002     C***
250900061002     C     *LIKE         DEFINE    UDATE8        DATEU8
251000061002     C     *LIKE         DEFINE    DSTPDR        WPADR
251100061002     C     *LIKE         DEFINE    dstFGS        WFIL
251200061002     C     *LIKE         DEFINE    dstNFV        WNFV
251300061002     C     *LIKE         DEFINE    dstDFV        WDFV
251400061002     C     *LIKE         DEFINE    dstDFV        WDFV2
251500061002     C     *LIKE         DEFINE    i33CNU        WCNU
251600061002     C     *LIKE         DEFINE    FTTFVL        WFVL
251700061002     C     *LIKE         DEFINE    FTTTSR        WTSR
251800061227     C     *LIKE         DEFINE    TBEKE1        KKE1
251900061227     C     *LIKE         DEFINE    TBECOD        KCOD1
252000061002     C*
252100061228     C* ACCESSO fidst
252200061011     C     Kdst1         KLIST
252300061010     C                   KFLD                    V1CFGS
252400061010     C                   KFLD                    WNFV
252500061011     C     Kdst2         KLIST
252600070410     C                   KFLD                    V1CFGS
252700070209     C                   KFLD                    WNPG
252800061011     C                   KFLD                    Wdfv2
252900080319     C     Kdst4         KLIST
253000080319     C                   KFLD                    wnpg
253100080319     C                   KFLD                    wnfv
253200080319     C                   KFLD                    v1cfgs
253300080319     C                   KFLD                    dsftrd
253400061002     C*
253500061228     C     Kdst          KLIST
253600070410     C                   KFLD                    V1CFGS
253700061002     C                   KFLD                    WNPG
253800061002     C                   KFLD                    Wnfv
253900070410     C     Kbrv          KLIST
254000070410     C                   KFLD                    WNPG
254100070410     C                   KFLD                    Wnfv
254200070410     C                   KFLD                    V1CFGS
254300061002     C* ACCESSO Fiqdt01l
254400061002     C     Kqdt          KLIST
254500061002     C                   KFLD                    V1CFGS
254600061002     C                   KFLD                    WNFV
254700061002     C* ACCESSO FIFTT02L + FIFST02L
254800061002     C     KFTT          KLIST
254900061002     C                   KFLD                    V2CPDR
255000061002     C                   KFLD                    WFVL
255100061002     C                   KFLD                    WDFV
255200061002     C                   KFLD                    WTSR
255300061002     C* ACCESSO FNORM06L
255400061002     C     KORM          KLIST
255500061002     C                   KFLD                    V1CFGS
255600061002     C                   KFLD                    wnfv
255700061002     C                   ENDSR
255800061002      *---------------------------------------------------------------*
255900061002      * Reperimento Dati del job (Utente/Operativi)                   *
256000061002      *---------------------------------------------------------------*
256100061002     c     DatiJob       BEGSR
256200061002      *
256300061002     c     *dtaara       define    §azute        azuteds
256400061002     c     *dtaara       define    §datiute      ddatiute
256500061002      *
256600061002     c                   in(E)     *dtaara
256700061002     c                   IF        %ERROR or RSUT = *blanks
256800061002     c                   clear                   Tibs34Ds
256900061002     c                   call      'TIBS34R'
257000061002     c                   parm                    Tibs34Ds
257100061002     c                   in        *dtaara
257200061002     c                   ENDIF
257300061002      *
257400061002     c                   ENDSR
257500950525     C*---------------------------------------------------------------*
257600950529     OFNLV22P   T    LRNLR
257700950523**  MSG
257800970416Foglio Distinta inesistente o in gestione ad altro punto operativo            01
257900950524Foglio Distinta al momento non disponibile: riprovare piu' tardi              02
258000950524Foglio Distinta gia' chiuso                                                   03
258100950628Il Foglio Distinta immesso non e' al momento manutenzionabile
258200950524Foglio Distinta non aperto con questo programma: impossibile la manutenzione  05
258300970416Autotrasportat. diverso da quello gia' presente in Distinta: ENTER per forzare
258400071214Impossibile modificare l'autotrasp.: presenti bolle/O.R.M. in distinta        07
258500970416Immettere il Codice Autotrasportatore                                         08
258600970416Con l'inserimento multiplo non e' possibile digitare il Codice Autotrasport.  09
258700970416Codice Autotrasportatore inesistente                                          10
258800970416P.O. Autotrasportatore incongruente con punto operativo apertura foglio
258900950524Tasto di funzione non ammesso se e' stato digitato il Foglio Distinta         12
259000950703Ora non e' possibile creare nuovi Fogli Distinta: attendere e ripremere F6    13
259100950525Impossibile annullare il Foglio Distinta: presenti bolle o spunte !!          14
259200950525Data immessa errata                                                           15
259300980430Data inferiore alla data corrente o superiore al limite imposto
259400950525Data errata: gia' confermati i conteggi !!                                    17
259500950525Immettere il Numero Bolle                                                     18
259600950526Il valore immesso per il campo non e' valido                                  19
259700950628La distinta e' ora in uso e non aggiornabile: attendere e ripremere F6        20
259800010314Impossibile annullare il Foglio Distinta: presenti O.R.M.!!                   21
259900020910Manca il numeratore dei Fogli Distinte: TELEFONARE IN SEDE!!!!                22
260000041008Incongruenza fra il numero dei bancali affidati e il flag di gestione         23
260100070516Non esiste un'anagrafica autotrasportatore completa. Occorre inserirla        24
260200061229Distinta non aggiornata. CONTROLLARE!!!                                       25
260300101022Filiale non ancora attiva per la gestione PDA scelta                          26
260400110224Scelta incongruente o non ammessa                                             27
260500080319La distinta è in una fase dove non è + possibile modificare il valore         28
260600160428Il turno è ammesso solo se tipo distinta è Parcel                             29
260700170227Autista BLOCCATO !!                                                           30
