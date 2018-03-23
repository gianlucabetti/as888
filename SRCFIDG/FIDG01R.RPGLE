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
006100170327     D FNLVF9ds      E DS
006200050519     D* DS PER FNLV24R - ricerca autotrasp.
006300050519     D fnlv24ds      E DS
006400061222     D fidg02ds      E DS
006500061229     D fidg05ds      E DS
006600070717     D fidg09ds      E DS
006700021203     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
006800021203     D DSLV50        E DS                  EXTNAME(FNLV50DS)
006900160720     D* DS PER Fidg31R - INTERROGAZIONE FOGLI VARI
007000160720     D fidg31ds      E DS
007100950529     D* DS PER FNLV25R - STAMPA ETICHETTA PADRONCINI
007200950529     D DSLV25        E DS                  EXTNAME(FNLV25DS)
007300980430     D**
007400020419     d ddgd          e ds
007500950908RDE  D*
007600030724RDE  D* DS PER FNLR59R - quadratura DISTINTA MANUALE e da spunte (x poste)
007700030724RDE  D FNLR59ds      E DS
007800020910
007900020910     d Trul33Ds      e ds
008000061227     d Fidg03ds      e ds
008100020910
008200950524     D*
008300950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008400950524     D WLBDAT          DS                  INZ
008500950524     D  G02DAT                 1      8  0
008600950524     D  G02INV                 9     16  0
008700950524     D  G02ERR                17     17
008800950524     D  G02TGI                18     22  0
008900020419      *
009000020419     d Wdata8          DS
009100020419     d  dadata                 1      8  0
009200020419     d  adata                  9     16  0
009300020419     d  GioLav                17     21  0
009400061002     d* schiere file fiapd20f
009500061002     D Ptc1            S               *
009600061002     D                                     INZ(%ADDR(apd2tc1))
009700061002     D tc1             S                   LIKE(apd2tc1)
009800061002     D                                     DIM(5)
009900061002     D                                     BASED(Ptc1)
010000061002     D Pts1            S               *
010100061002     D                                     INZ(%ADDR(apd2ts1))
010200061002     D ts1             S                   LIKE(apd2ts1)
010300061002     D                                     DIM(2)
010400061002     D                                     BASED(Pts1)
010500061002     d* schiere file fidst00f
010600061002     D Ptc1d           S               *
010700061002     D                                     INZ(%ADDR(dsttc1))
010800061002     D tc1d            S                   LIKE(dsttc1)
010900061002     D                                     DIM(5)
011000061002     D                                     BASED(Ptc1d)
011100061002     D Pts1d           S               *
011200061002     D                                     INZ(%ADDR(dstts1))
011300061002     D ts1d            S                   LIKE(dstts1)
011400061002     D                                     DIM(2)
011500061002     D                                     BASED(Pts1d)
011600950525     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRGI8 -
011700950525     D WGIDAT          DS                  INZ
011800950525     D  GIODAT                 1      8  0
011900950525     D  GIOINV                 9     16  0
012000950525     D  GIOTGI                17     21  0
012100950523     D*
012200950525     D                 DS
012300950525     D  AA                     1      4  0
012400950525     D  MM                     5      6  0
012500950525     D  GG                     7      8  0
012600950525     D  WDATA                  1      8  0
012700950523     D                 DS
012800950523     D  V1CFPD                 1      3  0
012900950523     D  V1CCPC                 4      7
013000950523     D  WPDR                   1      7
013100950523     D*
013200950524     D DS1C          E DS
013300900514     D KPJBA         E DS
013400170206     D*
013500170206     d   savpjbu       s                   like(kpjbu)
013600170206     D fnlr50wDS     E DS
013700170206     D*
013800061002     d* Parametri x Controllo profilo utenti
013900061002     d TIBS34ds      e ds
014000061002     d AZUTEds       e ds                  extname(AZUTE00F)
014100160720     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
014200061002     d* Ds per dati organigramma
014300061002     d DDatiUte      e ds
014400061011     D TIBS02        E DS                  EXTNAME(TIBS02DS)
014500010314     D DFAR          E DS
014600010314     D  §FARDS       E                     EXTFLD(D§FARDES)
014700010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
014800010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
014900010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
015000010314     D  §FARST       E                     EXTFLD(D§FARSTP)
015100010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
015200010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
015300950526     D*
015400950525     D* DEFINIZIONE COSTANTI
015500940712     D CINSER          C                   CONST(' Inserimento ')
015600940712     D CVARIA          C                   CONST(' Variazione  ')
015700940712     D CANNUL          C                   CONST('  Annullato  ')
015800160427     d Put             C                   1
015900160427     d EndPut          C                   2
016000160426     D Ordinamento     S              1
016100160427     d Numerico        C                   8
016200160427     d Carattere       C                   6
016300160427     d Ascendente      C                   1
016400160427     d Get             C                   3
016500160427     d NotUsed         S             16A
016600160427     d ReturnSize      S              9B 0
016700160427     d SizeList        S              9B 0
016800160428     d MaxKey          C                   2
016900160427     d stmrec1         S              4  0
017000160426      *  1=Autista
017100160426      *  2=RagSociale
017200160426      *  3=turno/codice
017300160426      ***
017400160426     d SflRcd          DS
017500160426     d  VSCPDR
017600160426     d  VSDPDR
017700160426     d  VSCTUR
017800160428     d  VSCTPM
017900160426     d  VSCFPP
018000160426     d  VSCPEP
018100160426     d  VSCDFV
018200160426     d  VSCPDA
018300160426     d  VSCTSTPDA
018400160428     d  VHRDFV
018500160428     d  VHCDFV
018600160428     d  VSCSCE
018700160426     d  VSCDSN
018800170207     d  hPdrForza
018900160426     d  Selected                      1A
019000160426
019100160426      /COPY QSYSINC/QRPGLESRC,QLGSORT
019200160426     d QLGKL                         16    DIM(MaxKey)
019300160426     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
019400160426     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
019500160426     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
019600160426     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
019700160426      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
019800160426      /COPY QSYSINC/QRPGLESRC,QUSEC
019900061002     C**********************************************************************
020000061002     C*
020100950523     C* WGES = 'F' ===> VADO A FINE PGM
020200950523     C     WGES          DOWNE     'F'
020300950523     C*
020400950523     C* WGES = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
020500950523     C     WGES          IFEQ      '1'
020600950523     C                   EXSR      GESD01
020700950523     C                   ENDIF
020800950523     C*
020900950524     C* WGES = '2' ===> GESTIONE SINGOLA VIDEATA DATI FOGLIO DISTINTA
021000950523     C     WGES          IFEQ      '2'
021100950523     C                   EXSR      GESD02
021200950523     C                   ENDIF
021300950523     C*
021400950523     C* WGES = '3' ===> GESTIONE VIDEATA SUBFILE DI TUTTE LE DISTINTE
021500950523     C     WGES          IFEQ      '3'
021600950523     C                   EXSR      GESS03
021700950523     C                   ENDIF
021800950523     C                   ENDDO
021900010208      *
022000020910
022100020910     c                   Clear                   Trul33Ds
022200020910     c                   Eval      I33Tla = 'C'
022300020910     c                   Movel(p)  Trul33Ds      Kpjbu
022400020910     c                   Call      'TRUL33R'
022500020910     c                   Parm                    Kpjba
022600020910
022700061227     c                   Clear                   Fidg03ds
022800061227     c                   Eval      D3Tla = 'C'
022900061227     c                   Movel(p)  Fidg03ds      Kpjbu
023000061227     c                   Call      'FIDG03R'
023100020910     c                   Parm                    Kpjba
023200070301      * MI RIPORTO IL NUMERO DISTINTA
023300070301     C     V1CNFP        IFGT      *ZEROS
023400070301     C                   CLEAR                   KPJBU
023500070301     C                   MOVEL     V1CNFP        KPJBU
023600070301     C                   ENDIF
023700950523     C*
023800950523     C                   SETON                                        LR
023900060929     C**********************************************************************
024000950523     C*--- GESTIONE VIDEATA DELLE SELEZIONI --------------------------*
024100060929     C**********************************************************************
024200950523     C     GESD01        BEGSR
024300950525     C                   SETOFF                                       010203
024400070515     C                   SETOFF                                       060908
024500950524     C                   CLEAR                   WUNO
024600050519     C                   CLEAR                   V05dfv
024700950525     C                   CLEAR                   V1CNFV
024800950525     C                   CLEAR                   V1CCPC
024801170919     C                   CLEAR                   wdfvex            8 0
024900950523     C*
025000950524     C* EMISSIONE FORMATO
025100060929     C                   do        *hival
025200950523     C                   EXFMT     LRF0D01
025300950523     C*
025400950523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
025500950523     C                   CLEAR                   V1CMSG
025600950523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
025700950523     C                   SETOFF                                       2840
025800950525     C                   SETOFF                                       414243
025900950523     C*
026000950523     C** CMD3  - FINE LAVORO
026100950526    1C     *INKC         IFEQ      *ON
026200950526     C                   MOVEL     'F'           WGES
026300060929     C                   leave
026400950526    1C                   ENDIF
026500950523     C*
026600950523     C** CMD18 - CAMBIO FILIALE
026700950526    1C     *INKS         IFEQ      *ON
026800950526     C                   SETON                                        04
026900060929     C                   iter
027000950526    1C                   ENDIF
027100950523     C*
027200950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
027300950524    1C     *IN04         IFEQ      *ON
027400950523     C                   MOVEL     V1CFGS        WFIL
027500950523     C                   EXSR      CTRFGS
027600060929     C   28              iter
027700950524    1C                   ENDIF
027800950523     C*
027900950523     C* CONTROLLI VIDEATA
028000950523     C                   EXSR      CTRD01
028100950524     C   28
028200060929     COR 90              iter
028300950524     C*
028400950524     C** CMD15 - INSERIMENTO MULTIPLO
028500950524    1C     *INKP         IFEQ      *ON
028600050519     c                   exsr      gesd05
028700950525     C                   SETON                                        03
028800950524     C                   MOVEL     '3'           WGES
028900060929     C                   leave
029000950524    1C                   ENDIF
029100950524     C*
029200950525     C* SE E' STATO INSERITO IL NUMERO DISTINTA IN PRIMA VIDEATA(02 ON)
029300950525     C*   PASSO ALLA GESTIONE DELLA VIDEATA DEL SINGOLO FOGLIO DISTINTA
029400950525    1C     *IN02         IFEQ      *ON
029500950524     C                   MOVEL     '2'           WGES
029600060929     C                   leave
029700950524    1C                   ENDIF
029800950526     C*
029900950526     C** ENTER - IMMISSIONE
030000950526     C                   SETON                                        01
030100060929     c                   MOVEL     '2'           WGES
030200060929     c                   leave
030300060929     c                   enddo
030400950526     C*
030500060929     C                   ENDSR
030600060929     C**********************************************************************
030700950523     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
030800060929     C**********************************************************************
030900950523     C     CTRFGS        BEGSR
031000950523     C                   CLEAR                   DSLV50
031100950523     C                   MOVEL     KNMUS         D50PRU
031200950523     C                   MOVEL     WFIL          D50FGS
031300950523     C                   CALL      'FNLV50R'
031400950523     C                   PARM                    DSLV50
031500950523     C*
031600950524    1C     D50ERR        IFNE      ' '
031700950523     C                   MOVEL     D50MSG        V1CMSG
031800950525     C                   SETON                                        4328
031900950524    1C                   ENDIF
032000950523     C*
032100060929     C                   ENDSR
032200060929     C**********************************************************************
032300950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
032400060929     C**********************************************************************
032500950523     C     CTRD01        BEGSR
032600950525     C                   SETOFF                                       010203
032700950525     C                   SETOFF                                       049015
032800950525     C                   CLEAR                   WBOL
032900960904     C                   CLEAR                   WSPU
033000010314     C                   CLEAR                   WORM
033100080320     C                   CLEAR                   faseorm           3
033200050519     c* ind 50 per video se p.o. gestisce Parcel+Messaggerie
033300050519     C     V1CFGS        CHAIN     AZORG01l
033400050519    1C                   IF        %found(azorg01l)
033500050519     C                   MOVEL     ORGdf0        og150
033600061010     C                   MOVEL     ORGde6        og146
033700080208     C                   MOVEL     ORGde8        og148
033800070723     C                   MOVEL     ORGde7        og147
033900080212     c* se procedura PDA non partita proteggo i flag
034000080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
034100080212     c                   setoff                                       96
034200080212     c                   else
034300080212     c                   seton                                        96
034400080212     c                   end
034500080212     c* ind. 11 x visualizzare i valori assegnati se EDP
034600080212     c                   if        %subst(knmus: 1: 3) = 'EDP'
034700080212     c                   seton                                        11
034800080212     c                   else
034900080212     c                   setoff                                       11
035000080212     c                   end
035100061010     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
035200061010     c* se data distinte automatiche < oggi p.o. partito
035300061010     c                   if        §ogdda = ' '
035400061010     c                   seton                                        lr
035500061010     c                   return
035600061010     c                   else
035700070109     c                   move      §ogdda        comdda            8 0
035800070330     c* ind. 11 x visualizzare i valori assegnati se EDP
035900070330     c                   if        %subst(knmus: 1: 3) = 'EDP'
036000070328     c                   seton                                        11
036100070328     c                   else
036200070328     c                   setoff                                       11
036300061010     c                   end
036400070328     c                   end
036500050519     c     §OGPKM        comp      'S'                                    50
036600160426     c     §OGturno      comp      'S'                                    54
036700050519   X1C                   ELSE
036800160426     c                   setoff                                       5054
036900050519    1C                   ENDIF
037000160426     c                   eval      vtesta ='1 |  AUTOTRASPORTATORE            -
037100160505     c                                       |M/P|Picking|Data dist.| -
037200160426     c                                       '
037300160505     c   50              eval      %subst(vtesta: 37: 4)='|P/M'
037400160505     c   54              eval      %subst(vtesta: 41: 4)='|Tur'
037500160426     c   96              eval      %subst(vtesta: 69: 8)='PDA TEST'
037600950607     C*
037700060929     c                   do
037800950607     C****  CODICE PADRONCINO  ****
037900950607     C* RICERCA
038000950607     C     '?'           SCAN      V1CCPC                                 90
038100950607    1C     *IN90         IFEQ      *ON
038200021203     c                   clear                   fnlv24ds
038300021203     C                   Z-ADD     V1CFGS        d24FIL
038400021203     C                   MOVEL     'R'           d24FLG
038500021203     C                   MOVEL     'A'           d24tip
038600021203     C                   MOVEL(p)  fnlv24ds      KPJBU
038700950607     C                   CALL      'FNLV24R'
038800950607     C                   PARM                    KPJBA
038900021203     C                   MOVEL     KPJBU         fnlv24ds
039000950607     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
039100021203     C     d24pdr        IFNE      0
039200021203     C                   MOVEL     d24pdr        WPDR
039300021203     C                   MOVEL     d24rsc        V2DPDR
039400950607     C                   ELSE
039500950607     C                   CLEAR                   V1CCPC
039600950607     C                   ENDIF
039700060929     C                   leave
039800950607    1C                   ENDIF
039900950607     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
040000950607    1C     V1CCPC        IFNE      *BLANKS
040100950607     C     V1CCPC        ANDNE     *ZEROS
040200950607     C                   TESTN                   V1CCPC               35
040300950607    2C     *IN35         IFEQ      *ON
040400950607     C                   MOVE      V1CCPC        W001              1
040500950607     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
040600950607    3C     W001          IFLT      '0'
040700950607     C                   SETOFF                                       35
040800950607    3C                   ENDIF
040900950607    2C                   ENDIF
041000950607    2C     *IN35         IFEQ      *OFF
041100950607     C                   MOVEL     MSG(10)       V1CMSG
041200950607     C                   SETON                                        4128
041300060929     C                   leave
041400950607    2C                   ENDIF
041500950607     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
041600950607     C                   MOVE(P)   WPDR          WPADR
041700061002     C     kapdw         CHAIN     fiapd01l
041800061002     c                   if        not %found(fiapd01l) or
041900061002     c                             (%found(fiapd01l) and apdatb <> ' ')
042000950607     C                   MOVEL     MSG(10)       V1CMSG
042100950607     C                   SETON                                        4128
042200060929     C                   leave
042300070123     c                   else
042400070123     c                   eval      dapdflr = apdflr
042500950607    1C                   ENDIF
042600061003    1C                   ENDIF
042700950523     C****  NUMERO DISTINTA  ****
042800950523     C* RICERCA
042900950523     C     '?'           SCAN      V1CNFV                                 90
043000950523    1C     *IN90         IFEQ      *ON
043100160720     C                   CLEAR                   fidg31ds
043200160720     C                   Z-ADD     0             fdggda
043300160727     C                   Z-ADD     0             fdggal
043400160720     C                   Z-ADD     V1CFGS        fdgFGS
043500160720     C                   MOVEL     'FIDG01R'     fdgPGM
043600160720     C                   MOVEL     'S'           fdgRIC
043700160720     C                   MOVEL     '2'           fdgTFV
043800160720     C                   Z-ADD     4             fdgNPG
043900160720     C                   MOVEL     fidg31ds      KPJBU
044000160720     c                   clear                   $FV6
044100160720      * impostato a tutti 99999
044200160720     c                   do        600           zz                3 0
044300160720     c                   z-add     999999        $FV6(zz)
044400160720     c                   enddo
044500160720     C                   CALL      'FIDG31R'
044600061228     C                   PARM                    KPJBA
044700160720     c                   parm                    $FV6
044800160720     C                   MOVEL     KPJBU         fidg31ds
044900070627     c                   movel     *all'0'       v1cnfv
045000160720     C                   move      fdgNFV        v1cnfv
045100060929     C                   leave
045200950523    1C                   ENDIF
045300950523     C*
045400950523    1C     V1CNFV        IFNE      *BLANKS
045500950523     C     V1CNFV        ANDNE     *ZEROS
045600950523     C                   TESTN                   V1CNFV               35
045700950523    2C     *IN35         IFEQ      *ON
045800950523     C                   MOVE      V1CNFV        W001
045900950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
046000950523    3C     W001          IFLT      '0'
046100950523     C                   SETOFF                                       35
046200950523    3C                   ENDIF
046300950523    2C                   ENDIF
046400950523    2C     *IN35         IFEQ      *OFF
046500950524     C                   MOVEL     MSG(1)        V1CMSG
046600950524     C                   SETON                                        4028
046700060929     C                   leave
046800950523    2C                   ENDIF
046900061002     C* SE NUMERO DISTINTA >< 0 CONTROLLO SE ESISTE IN Fidst00F
047000061010     C                   MOVE      V1CNFV        WNFV
047100070410     C     Kdst          CHAIN     fidst09l                             39
047200950523     C* 39 ON  - RECORD ALLOCATO
047300950524    2C     *IN39         IFEQ      *ON
047400950523     C                   MOVEL     MSG(2)        V1CMSG
047500950524     C                   SETON                                        4028
047600060929     C                   leave
047700950524    2C                   ENDIF
047800950523     C*
047900070410    2C                   IF        %found(fidst09l)
047901170919     c                   movel     dstdfv        wdfvex
048000080207     c                   movel     dstflr        ddstflr
048100061227     C* FOGLIO DISTINTA GIA' CHIUSO
048200061227    3C     dstFCF        IFEQ      'S'
048300061227     C                   MOVEL     MSG(3)        V1CMSG
048400061227     C                   SETON                                        4028
048500061227     C                   leave
048600061228   X3C                   ELSE
048700061228    4C     dstFCF        IFNE      ' '
048800950524     C* FOGLIO DISTINTA NON MANUTENZIONABILE
048900061228     C                   MOVEL     MSG(4)        V1CMSG
049000061228     C                   SETON                                        4028
049100061228     C                   leave
049200061228    4C                   ENDIF
049300061228    3C                   ENDIF
049400080319     C*fasi distinta
049500080319     c                   exsr      srfasi
049600950525     C* 06 ON  - FOGLIO DISTINTA ANNULLATO: ABILITO CMD5-RIATTIVAZIONE
049700950525     C     DSTATB        IFNE      ' '
049800950525     C                   MOVEL     CANNUL        V1CDES
049900950525     C                   SETON                                        06
050000950530     C* SE FOGLIO DISTINTA ANNULLATO NON CONSIDERO IL PADRONCINO
050100950530     C                   CLEAR                   V1CCPC
050200950529     C                   ELSE
050300950529     C                   MOVEL     CVARIA        V1CDES
050400950525     C                   ENDIF
050500950525   X3C                   ELSE
050600950524     C* NUMERO DISTINTA INESISTENTE
050700950524     C                   MOVEL     MSG(5)        V1CMSG
050800950524     C                   SETON                                        4028
050900060929     C                   leave
051000950524    3C                   ENDIF
051100060929     C* SE esiste Fiqdt00F
051200070109     C     Kqdt          CHAIN(n)  Fiqdt01l                           27
051300950524     C* CONTROLLO SE ESISTONO BOLLE SUL FOGLIO DISTINTA DIGITATO
051400070312     c                   setoff                                       31
051500070424     c                   if        comdda= 20391231 or comdda > dateu8
051600070223     c                   Z-Add     V1cfgs        Kargfgs
051700070223     C                   Z-ADD     WNFV          Kargndc
051800070223     C     Karg          SETLL     Fiarg03l                               31
051900070312     c                   end
052000070312     c                   if        not *in31
052100070223     c                   Z-Add     V1cfgs        Karbifp
052200070223     C                   Z-ADD     WNFV          Karbndc
052300070223     C     Karb78        SETLL     FNARB78L                               31
052400070223     c                   end
052500950525     C   31              MOVEL     'S'           WBOL              1
052600070410     C     Kbrv          SETLL     FnBRV01L                               31
052700960904     C   31              MOVEL     'S'           WSPU              1
052800010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
052900080319    2C*    WBOL          IFEQ      *BLANKS
053000080319     C*    WSPU          ANDEQ     *BLANKS
053100010314     C     KORM          SETLL     FNORM06L
053200010314     C     KORM          READE     FNORM06L                               31
053300010314    3C     *IN31         DOWEQ     *OFF
053400010314     C                   CLEAR                   TIBS02
053500010314     C                   CLEAR                   DFAR
053600010314     C                   MOVEL     'C'           T02MOD
053700010314     C                   MOVEL     KNSIF         T02SIF
053800010314     C                   MOVEL     'FAR'         T02COD
053900010314     C                   MOVEL     ORMFAO        T02KE1
054000010314     C                   CALL      'TIBS02R'
054100010314     C                   PARM                    KPJBA
054200010314     C                   PARM                    TIBS02
054300010314     C     T02ERR        IFEQ      *BLANKS
054400010314     C                   MOVEL     T02UNI        DFAR
054500010314     C                   ENDIF
054600010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
054700010314     C* DISTINTA E' PRESENTE UN O.R.M.
054800010314    4C     §FARAF        IFNE      'S'
054900010314     C                   SETON                                        31
055000010314     C                   MOVEL     'S'           WORM              1
055100010314   X4C                   ELSE
055200010314     C     KORM          READE     FNORM06L                               31
055300010314    4C                   ENDIF
055400010314    3C                   ENDDO
055500080319    2C*                  ENDIF
055600950524     C*
055700950524    2C     V1CCPC        IFEQ      *BLANKS
055800950524     C     V1CCPC        OREQ      *ZEROS
055900960904     C     WBOL          IFEQ      'S'
056000960904     C     WSPU          OREQ      'S'
056100071214     C     Worm          orEQ      'S'
056200960904     C                   SETON                                        15
056300960904     C                   ENDIF
056400950524   X2C                   ELSE
056500950525     C                   MOVE(P)   WPDR          WPADR
056600950524    3C     WPADR         IFEQ      DSTPDR
056700960904     C     WBOL          IFEQ      'S'
056800960904     C     WSPU          OREQ      'S'
056900071214     C     Worm          orEQ      'S'
057000960904     C                   SETON                                        15
057100960904     C                   ENDIF
057200950524   X3C                   ELSE
057300950524     C* PADRONCINO DIVERSO DA QUELLO IN DISTINTA: ENTER PER FORZARE
057400960904    4C     WBOL          IFEQ      ' '
057500071214     C     Worm          andeq     ' '
057600950524    5C     WUNO          IFEQ      ' '
057700950524     C                   MOVEL     MSG(6)        V1CMSG
057800950524     C                   SETON                                        4128
057900950524     C                   MOVEL     '1'           WUNO              1
058000060929     C                   leave
058100950524    5C                   ENDIF
058200950524   X4C                   ELSE
058300950524     C                   MOVEL     MSG(7)        V1CMSG
058400950524     C                   SETON                                        4128
058500060929     C                   leave
058600950524    4C                   ENDIF
058700950524    3C                   ENDIF
058800950524    2C                   ENDIF
058900060929     C* SE E' STATO INSERITO IL NUMERO DISTINTA DEVO INIBIRE
059000061002     C* IL CMD15-INSERIMENTO MULTIPLO
059100950526     C     *INKP         IFEQ      *ON
059200950524     C                   MOVEL     MSG(12)       V1CMSG
059300950524     C                   SETON                                        28
059400060929     C                   leave
059500950524    2C                   ENDIF
059600060929     C* 02 ON  - INSERITO la DISTINTA IN PRIMA VIDEATA QUINDI
059700061002     C* SONO IN MANUTENZIONE
059800950525     C                   SETON                                        02
059900950523   X1C                   ELSE
060000950523     C* NUMERO DISTINTA = 0
060100950524    2C     V1CCPC        IFEQ      *BLANKS
060200950523     C     V1CCPC        OREQ      *ZEROS
060300061002     C* SE SI VUOLE INSERIRE UNA DISTINTA OCCORRE IMMETTERE IL
060400061002     C* CODICE PADRONCINO
060500950526    3C     *INKP         IFEQ      *OFF
060600950524     C                   MOVEL     MSG(8)        V1CMSG
060700950523     C                   SETON                                        4128
060800060929     C                   leave
060900950526    3C                   ENDIF
061000950524     C*
061100950524   X2C                   ELSE
061200950524     C* SE PREMUTO CMD15-INSERIMENTO MULTIPLO IL COD.PADRONCINO DEVE
061300061002     C* RIMANERE VUOTO
061400950524    3C     *INKP         IFEQ      *ON
061500950524     C                   MOVEL     MSG(9)        V1CMSG
061600950524     C                   SETON                                        4128
061700060929     C                   leave
061800950524    3C                   ENDIF
061900020910      * Il p.o. del padroncino deve essere uguale al p.o. in gestione
062000020910     c                   If        v1cfpd <> v1cfgs
062100020910     c                   Eval      V1cmsg = Msg(11)
062200020910     c                   Eval      *In41 = *On
062300020910     c                   Eval      *In28 = *On
062400060929     c                   leave
062500020910     c                   EndIf
062600950524    2C                   ENDIF
062700950524     C*
062800950523    1C                   ENDIF
062900060929     c                   enddo
063000061002     C*
063100060929     C                   ENDSR
063200080319     C*-----------------------------------------------------*
063300080319     C* controllo fasi distinta per dati scaricati a PDA         *
063400080319     C*-----------------------------------------------------*
063500080319     C     srfasi        BEGSR
063600080319     c* controllo se i flag per PDA e mat/pom sono modificabili
063700080319     c                   clear                   fasedst           3
063800080319     c                   setoff                                       7273
063900110223     c* se ho la fase CUS (ok da spc) non faccio manutenzionare i flag
064000110223     c* dati a pda e matt/pom sia x con che x orm
064100110223     c*                  eval      dsftrd = 'CUS'
064200110223     C*    KDST4         chain     fidsf02l
064300110223     c*                  if        %found(fidsf02l)
064400110223     c*                  eval      fasedst = dsftrd
064500110223     c*                  seton                                        7273
064600110223     c*                  end
064700110223     c* se ho la fase PDR (preparato x pda ORM)
064800110223     c* non faccio manutenzionare il flag matt/pom
064900110223     c                   eval      dsftrd = 'PDR'
065000110223     C     KDST4         chain     fidsf02l
065100110223     c                   if        %found(fidsf02l)
065200110223     c                   eval      faseorm = dsftrd
065300110223     c                   seton                                        73
065400110223     c                   end
065500080319     c* se ho la fase PDC (preparato x pda con) o PDO x orm
065600080319     c* non faccio manutenzionare il flag matt/pom
065700080319     c                   eval      dsftrd = 'PDC'
065800080326     C     KDST4         chain     fidsf02l
065900080326     c                   if        %found(fidsf02l)
066000080319     c                   eval      fasedst = dsftrd
066100080319     c                   seton                                        73
066200080326     c                   end
066300080319     c*
066400080319     C                   ENDSR
066500060929     C**********************************************************************
066600950524     C*--- GESTIONE SINGOLA VIDEATA DATI FOGLIO DISTINTA -------------*
066700060929     C**********************************************************************
066800950524     C     GESD02        BEGSR
066900950525     C* SE SONO IN IMMISSIONE PRELEVO NUMERATORE FOGLIO DISTINTA
067000950526    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
067100950525     C     *IN03         OREQ      *ON                                          *CMD15
067200950525     C                   MOVEL     CINSER        V1CDES
067300950703     C                   CLEAR                   V2CNFV
067400950525    1C                   ENDIF
067500950524     C* IMPOSTAZIONE DATI VIDEATA
067600950524     C                   EXSR      RIED02
067700950524     C* EMISSIONE FORMATO
067800060929     C                   do        *hival
067900950524     C                   EXFMT     LRF0D02
068000950524     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
068100950524     C                   CLEAR                   V2CMSG
068200950524     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
068300950525     C                   SETOFF                                       284546
068400041008     C                   SETOFF                                       474849
068500160426     C                   SETOFF                                       515255
068600170206     C                   SETOFF                                       44
068700950524     C** CMD12 - RITORNO
068800950525    1C     *INKL         IFEQ      *ON
068900950524     C                   MOVEL     '1'           WGES
069000061006     C                   rolbk
069100950526    2C     *IN01         IFEQ      *ON                                          *IMMISSIONE
069200950525     C     *IN03         OREQ      *ON                                          *CMD15
069300950529     C                   CLEAR                   V1CNFP
069400061011     C                   CLEAR                   V2CNfv
069500950525   X2C                   ELSE
069600070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
069700950525    2C                   ENDIF
069800060929     C                   leave
069900950525    1C                   ENDIF
070000950525     C** CMD16 - ANNULLAMENTO  FOGLIO DISTINTA
070100950525    1C     *INKQ         IFEQ      *ON
070200950524     C                   EXSR      ANNDIS
070300061229     c* aggiorno archivi FNFVV e FNDST
070400061229     c                   if        not *in28
070500061229     c                   eval      d05cmd ='KQ'
070600061229     c                   exsr      sraggold
070700061229     c                   end
070800061229     C   28              iter
070900061002     c                   commit
071000950524     C                   MOVEL     '1'           WGES
071100070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
071200060929     C                   leave
071300950525    1C                   ENDIF
071400950525     C** CMD5  - RIATTIVAZIONE FOGLIO DISTINTA
071500950525    1C     *INKE         IFEQ      *ON
071600950525     C* RIPRISTINO IL RECORD IN FNDST00F
071700061228     c                   z-add     4             wnpg
071800070410     C     KDST          chain     fidst09l
071900070410     c                   if        %found(fidst09l)
072000061228     C                   CLEAR                   DSTATB
072100061228     C                   UPDATE    FiDST000
072200061228     c                   end
072300070717     c* Pianificazione consegne
072400061222     C     KDST1         setll     FiDST31l                           30
072500061222     c                   do        *hival
072600061222     C     KDST1         reade     FiDST31l
072700061222     c                   if        %eof(fidst31l)
072800061222     c                   leave
072900061222     c                   end
073000061222     C                   CLEAR                   DST3ATB
073100061222     C                   UPDATE    FiDST300
073200061222     c                   enddo
073300070717     c* pianificazione ORM
073400070726     c                   if        §OGDDAO <> ' '
073500070717     C     KDST1         setll     FiDST51l                           30
073600070717     c                   do        *hival
073700070717     C     KDST1         reade     FiDST51l
073800070717     c                   if        %eof(fidst51l)
073900070717     c                   leave
074000070717     c                   end
074100070717     C                   CLEAR                   DST5ATB
074200070717     C                   UPDATE    FiDST500
074300070717     c                   enddo
074400070726     c                   end
074500041005     C* RIPRISTINO IL RECORD IN Fiqdt01l
074600070313     c                   if        comdda <> 0
074700041005     C     Kqdt          chain     fiqdt01l                           30
074701171003      * nn ho trovato il rekord provo anche con data 999999999 altrimenti rischio di raddoppiare
074702171003      * i rekord di rientro
074703171003     c                   if        *in30
074704171003     c                   move      *all'9'       wdfvex
074705171003     c     kqdt          chain     fiqdt01l                           30
074706171003     c                   if        not *in30
074707171003     c                   clear                   QDTDAECO
074708171003     c                   clear                   QDTORACO
074709171003     c                   move      dstdfv        QDTddc
074710171003     c                   endif
074711171003     c                   endif
074712171003      *
074800041005     C                   clear                   qdtatb
074900041005     C  n30              update    fiqdt000
075000070109     c                   end
075100950525     C                   MOVEL     '1'           WGES
075200070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
075300061229     c* aggiorno archivi FNFVV e FNDST
075400061229     c                   eval      d05cmd ='KE'
075500061229     c                   exsr      sraggold
075600061229     C   28              iter
075700061002     c                   commit
075800060929     C                   leave
075900950525    1C                   ENDIF
076000170207      ***
076100950525     C* CONTROLLI VIDEATA
076200170207      ***
076300170207     C  n28              EXSR      CTRD02
076400170208     C** CMD12 - RITORNO dalla window di disaccreditato
076500170208    1C     *IN44         IFEQ      *ON
076600170208     C                   SETOFF                                       44
076700170208     C                   MOVEL     '1'           WGES
076800170208     C                   rolbk
076900170208    2C     *IN01         IFEQ      *ON                                          *IMMISSIONE
077000170208     C     *IN03         OREQ      *ON                                          *CMD15
077100170208     C                   CLEAR                   V1CNFP
077200170208     C                   CLEAR                   V2CNfv
077300170208   X2C                   ELSE
077400170208     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
077500170208    2C                   ENDIF
077600170208     C                   leave
077700170208    1C                   ENDIF
077800170207      *---
077900060929     C   28              iter
078000950525     C** CMD6  - AGGIORNA
078100070928     c                   if        *inkf or *inkh or *inki
078200061011     c                             or *inkb
078300061002     C                   EXSR      AGGD02
078400060929     C   28              iter
078500070928     c                   if        *inkf or *inkh or *inki
078600070315     c                             or *inkb
078700061229     c* aggiorno archivi FNFVV e FNDST
078800061229     c                   eval      d05cmd ='KF'
078900061229     c                   exsr      sraggold
079000061229     C   28              iter
079100070315     c                   if        not *inkb
079200061006     c                   commit
079300061229     c                   end
079400070315     c                   end
079500061006     c                   end
079600070122     C** CMD2  - altri dati distinata
079700061002     c                   if        *inkb
079800061002     C                   exsr      sraltro
079900070122     c* se nel pgm fidg02r è stato premuto cmd di conferma commit ed
080000061011     c* esco
080100061222     c                   if        d02cmde = 'KF'
080200070322     c                   eval      *inkf = '1'
080300070315     c                   commit
080400070314     c                   else
080500070322     c                   iter
080600061011     c                   end
080700061002     c                   end
080800070928     C** CMD9  - AGGIORNA e chiama gestione e quadratura della singola
080900070928     C**         distinta
081000070928    1C     *INKi         IFEQ      *ON
081100070928     c* emetto window per chiedere la stampante  laser o ad aghi
081200070928     C                   EXSR      wind60
081300070928     c                   if        laser = ' '
081400070928     c                   seton                                        28
081500070928     c                   clear                   V2CMSG
081600070928     C                   iter
081700070928     c                   else
081800070928RDE  C                   CLEAR                   FNLR59ds
081900070928     C                   eval      d59lsr = laser
082000070928RDE  C                   MOVE      V2CFGS        D59fgs
082100070928RDE  C                   MOVE      V2CNFV        D59NDD
082200070928RDE  C                   MOVE      V2CNFV        D59NDA
082300070928RDE  C                   MOVE      V2CDFV        Dtaeur
082400070928RDE  C                   MOVE      dtaeur        Dtaiso
082500070928RDE  C                   MOVE      dtaiso        D59ddt
082600070928RDE  C                   MOVEL(p)  V2dPDR        D59dsf
082700070928RDE  C                   MOVE      V2CPDR        D59pdr
082800070928RDE  C                   MOVEl     V2Ctpm        D59tpm
082900160428RDE  C                   MOVEl     V2Ctur        D59tur1
083000160428RDE  C                   clear                   D59tur2
083100070928RDE  C                   clear                   D59tpd
083200070928RDE  C                   MOVEL     V2CFPP        D59FPP
083300070928     C* STAMPA SOLO LE ANOMALIE
083400070928     C                   MOVEL     'A'           d59STP
083500070928     C* NON STAMPA ORM
083600070928     C                   MOVEL     'N'           d59SOR
083700070928RDE  C                   MOVEL(p)  FNLR59ds      KPJBU
083800070928RDE  C                   CALL      'FNLRF7C'
083900070928RDE  C                   PARM                    KPJBA
084000070928     c                   end
084100070928     c                   end
084200061002     C* SE SONO IN IMMISSIONE(N02) CONTROLLO SE RICHIESTA LA STAMPA
084300070328     C* CMD8 - AGGIORNA E STAMPA ETICHETTA PADRONCINO (IN VARIAZIONE)
084400070328    1C     *INKH         IFEQ      *ON
084500070328     C     *IN02         oreq      *OFF
084600950529     C     *INKF         ANDEQ     *ON
084700950529     C     V1CSTE        ANDEQ     'S'
084800950529     C                   CLEAR                   DSLV25
084900950529     C                   MOVEL     V2CPDR        D25PDR
085000950529     C                   MOVEL     V2DPDR        D25RSC
085100070328     C                   eval      d25nfv = V2CNFV
085200950529     C                   MOVEL     V2CDFV        D25DDC
085300080310     c                   eval      D25fpp = V2Cfpp
085400950529     C                   MOVEL     V2CNOT        D25NOT
085500950908     C                   CALL      PA1PSD
085600950529     C                   PARM                    DSLV25
085700950525    1C                   ENDIF
085800070928     c                   if        *inkf or *inkh or *inki
085900070328     c                   leave
086000070328     c                   end
086100950525     C*
086200061002     c                   enddo
086300950525     C*
086400950525     C                   MOVEL     '1'           WGES
086500070328     C                   MOVE      V2CNFV        V1CNFP                         *DIST.PRECEDENTE
086600060929     c*
086700060929     C                   ENDSR
086800070928     C*--------------------------------------------------------------*
086900070928     C*  richiesta tipo stampante
087000070928     C*--------------------------------------------------------------*
087100070928     C     wind60        BEGSR
087200070928     C*
087300070928     c                   clear                   laser             1
087400070928     c                   call      'FNLRF6R1'
087500070928     c                   parm                    laser
087600070928     C*
087700070928     C                   ENDSR
087800060929     C**********************************************************************
087900061002     C*--- PRELEVO NUMERATORE DISTINTA CONSEGNA ---------------*
088000060929     C**********************************************************************
088100950524     C     CRTNFV        BEGSR
088200950703     C                   SETOFF                                       90
088300070122      * se sono in inserimento però ho già un numero di distinta impostato
088400070122      * Controllo che non esista già un foglio con lo stesso numero
088500070122      * richiamo il pgm passando come tipo foglio 8 così controlla anche l'esistenza
088600070122      * del tipo 8
088700070122    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
088800070122     C     *IN03         OREQ      *ON                                          *CMD15
088900070122     c                   if        v2cnfv <> 0
089000070122     c                   Clear                   Fidg03ds
089100070122     c                   Eval      D3Npg = 8
089200070122     c                   Eval      D3Fgs = V2cfgs
089300070122     c                   Z-Add     v2cnfv        D3Ndt
089400070122     c                   Movel(p)  Fidg03ds      Kpjbu
089500070122     c                   Call      'FIDG03R'
089600070122     c                   Parm                    Kpjba
089700070122     c                   Movel     Kpjbu         Fidg03ds
089800070122     c* se ritorno ='N' vuol dire che non ha trovato il numero già usato o
089900070122     c* come foglio o in una bolla ed utilizzo quello
090000070122     c* altrimenti proseguo e ne prelevo uno
090100070122If  2c                   If        D3Flg = 'N'
090200070122     c                   leavesr
090300070122    2c                   EndIf
090400070122    2c                   EndIf
090500070122    2c                   EndIf
090600061002     c*
090700020910Do  1c                   Do        *Hival
090800020910      * Cerco il numero da staccare
090900020910     c                   Clear                   Trul33Ds
091000020910     c                   Eval      I33Cnu = Wcnu
091100020910     c                   Eval      I33Po1 = V2cfgs
091200020910     c                   Eval      I33Num = 1
091300020910     c                   Movel(p)  Trul33Ds      Kpjbu
091400020910     c                   Call      'TRUL33R'
091500020910     c                   Parm                    Kpjba
091600020910     c                   Movel     Kpjbu         Trul33Ds
091700020910
091800020910S   2c                   Select
091900020910      * Numeratore non trovato
092000020910     c                   When      O33err =  01
092100020910     c                   Eval      V2cmsg = Msg(22)
092200020910     c                   Eval      *In28 = *On
092300020910     c                   Eval      *In90 = *On
092400060929     c                   leave
092500020910      * Numeratore allocato
092600020910     c                   When      O33err =  02
092700020910     c                   Eval      V2cmsg = Msg(13)
092800020910     c                   Eval      *In28 = *On
092900020910     c                   Eval      *In90 = *On
093000060929     c                   leave
093100020910
093200020910     c                   When      O33err <> *zeros
093300020910     c                   Eval      V2cmsg = O33Msg
093400020910     c                   Eval      *In28 = *On
093500020910     c                   Eval      *In90 = *On
093600020910    2c                   EndSl
093700020910
093800020910      * Controllo che non esista già un foglio con lo stesso numero
093900020919      * richiamo il pgm passando come tipo foglio 8 così controlla anche l'esistenza
094000020919      * del tipo 8
094100061227     c                   Clear                   Fidg03ds
094200061227     c                   Eval      D3Npg = 8
094300061227     c                   Eval      D3Fgs = V2cfgs
094400061227     c                   Z-Add     O33Nrf        D3Ndt
094500061227     c                   Movel(p)  Fidg03ds      Kpjbu
094600061227     c                   Call      'FIDG03R'
094700061227     c                   Parm                    Kpjba
094800061227     c                   Movel     Kpjbu         Fidg03ds
094900020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
095000020910     c* come foglio o in una bolla
095100061227If  2c                   If        D3Flg <> 'N'
095200061227     c                   Iter
095300061227    2c                   EndIf
095400020910     c                   Z-Add     O33Nrf        V2cnfv
095500020910     c                   Leave
095600020910    1c                   EndDo
095700050426     C*
095800060929     C                   ENDSR
095900060929     C**********************************************************************
096000950524     C*--- IMPOSTAZIONE DATI VIDEATA ---------------------------------*
096100060929     C**********************************************************************
096200950524     C     RIED02        BEGSR
096300950524     C* FILIALE GESTIONE E DECODIFICA
096400950524     C                   MOVEL     V1CFGS        V2CFGS
096500950524     C     V2CFGS        CHAIN     AZORG                              30
096600950525    1C     *IN30         IFEQ      *OFF
096700950524     C                   MOVEL     ORGDES        V2DFGS
096800050519     c* ind 50 per video se p.o. gestisce Parcel+Messaggerie
096900050519     C                   MOVEL     ORGdf0        og150
097000050519     c     §OGPKM        comp      'S'                                    50
097100160426     c     §OGturno      comp      'S'                                    54
097200950525   X1C                   ELSE
097300950524     C                   CLEAR                   V2DFGS
097400160426     c                   setoff                                       5054
097500950525    1C                   ENDIF
097600950526     C****  I M M I S S I O N E  ****
097700950525    1C     *IN01         IFEQ      *ON
097800950525     C                   MOVEL     WPDR          V2CPDR
097900950525     C                   MOVEL     APDRSC        V2DPDR
098000070720     C                   MOVEL     §APDpda       V2cpda
098100080207     C                   MOVEL     §APDtstpda    V2ctstpda
098200110311     c                   move      *zeros        v2cnbn
098300110311     c                   move      apdptp        v2cnbn
098400950525     C                   Z-ADD     UDATE8        V2CDFV                         *DATA DIST.
098500070720     c                   if        v2cpda = ' '
098600070720     c                   eval      v2cpda = 'N'
098700070720     c                   end
098800950607     C* CONTROLLI
098900160427     C                   clear                   vhcdfv
099000160427     C                   clear                   vhrdfv
099100950607     C                   EXSR      CTRDDC
099200061227     C* I CAMPI PRESTAZIONE PADRONCINO (MATT/POM/gg) E PIKING (S/N) LI
099300950525     C*   PROPONGO PRENDENDOLI DALL'ANAGRAFICA
099400061228     C                   MOVEL     'S'           V2CFCA                         *MATTINO
099500950525    2C     HHMM          IFLE      1200
099600950525     C                   MOVEL     'M'           V2CFPP                         *MATTINO
099700950525    3C     APDPKM        IFEQ      ' '
099800950525     C                   MOVEL     'N'           V2CPEP
099900950525   X3C                   ELSE
100000950525     C                   MOVEL     APDPKM        V2CPEP
100100950525    3C                   ENDIF
100200061228    3C     APDcam        IFEQ      ' '
100300061228     C                   MOVEL     'N'           V2Cfca
100400061228   X3C                   ELSE
100500061228     C                   MOVEL     APDcam        V2Cfca
100600061228    3C                   ENDIF
100700950525   X2C                   ELSE
100800950525     C                   MOVEL     'P'           V2CFPP                         *POMERIGGIO
100900950525    3C     APDPKP        IFEQ      ' '
101000950525     C                   MOVEL     'N'           V2CPEP
101100950525   X3C                   ELSE
101200950525     C                   MOVEL     APDPKP        V2CPEP
101300950525    3C                   ENDIF
101400061228    3C     APDcap        IFEQ      ' '
101500061228     C                   MOVEL     'N'           V2Cfca
101600061228   X3C                   ELSE
101700061228     C                   MOVEL     APDcap        V2Cfca
101800061228    3C                   ENDIF
101900950525    2C                   ENDIF
102000050519     c* parcel/messaggerie
102100050519     C                   MOVEL     apdflr        dapdflr                        *MATT/POM
102200150804     c                   if        *in50
102300150804     C                   MOVEL     §apdtpm       v2ctpm                         *MATT/POM
102400150804     c                   else
102500150804     c                   clear                   v2ctpm
102600150804     c                   end
102700050523     c                   if        v2ctpm = ' '
102800050523     c                   eval      v2ctpm = 'P'
102900050523     c                   end
103000160426     c* turno
103100160426     c                   if        *in54
103200160426     C                   MOVEL     §apdturno     v2ctur                         *MATT/POM
103300160426     c                   else
103400160426     c                   clear                   v2ctur
103500160426     c                   end
103600160427     c                   if        v2ctur = ' ' and §apdtpm = ' '
103700160426     c                   eval      v2ctur = '1'
103800160426     c                   end
103900950525     C                   CLEAR                   V2CNOT                         *NOTE
104000950525    1C                   ENDIF
104100950525     C****  M A N U T E N Z I O N E  ****
104200950525    1C     *IN02         IFEQ      *ON                                          *VARIAZIONE
104300950525     C     *IN06         OREQ      *ON                                          *ANNULLATO
104400061002     C* PRENDO I DATI DALLA VIDEATA DELLE SELEZIONI
104500950525    2C     V1CCPC        IFNE      *BLANKS
104600950525     C     V1CCPC        ANDNE     *ZEROS
104700950525     C                   MOVEL     WPDR          V2CPDR
104800950525     C                   MOVEL     APDRSC        V2DPDR
104900070720     C                   MOVEL     §apdpda       V2cpda
105000080207     C                   MOVEL     §apdtstpda    V2ctstpda
105100950525   X2C                   ELSE
105200950525     C* PRENDO I DATI AGGANCIANDO L'ANAGRAFICA PADRONCINI
105300021203     C     kapdd         CHAIN     fiapd000                           32
105400950525    3C     *IN32         IFEQ      *OFF
105500070123     c                   eval      dapdflr = apdflr
105600950525     C                   MOVEL     APDPDR        V2CPDR
105700950525     C                   MOVEL     APDRSC        V2DPDR
105800070123     C                   MOVEL     APDflr        dapdflr
105900070717     C                   MOVEL     §APDpda       V2cpda
106000080207     C                   MOVEL     §APDtstpda    V2ctstpda
106100950525   X3C                   ELSE
106200950525     C                   CLEAR                   V2CPDR
106300950525     C                   CLEAR                   V2DPDR
106400080207     C                   CLEAR                   V2ctstpda
106500070717     C                   CLEAR                   V2cpda
106600950525    3C                   ENDIF
106700950525    2C                   ENDIF
106800160428     c                   setoff                                       10
106900061002     C                   eval      v2cnfv = DSTNFV                              *NUM. DIST.
107000061002     C                   move      dstDFV        dtaiso
107100061002     C                   move      dtaiso        dtaeur
107200061002     C                   MOVE      dtaeur        V2CDFV
107300060929     C* NEL CAMPO CHE SAREBBE IDDEN CI METTO LA DATA distinta
107400950607     C                   MOVEL     V2CDFV        VHCDFV
107500150804     c                   if        *in50
107600150804     C                   MOVEL     dsttpm        v2ctpm                         *MATT/POM
107700150804     c                   else
107800150804     C                   clear                   v2ctpm                         *MATT/POM
107900150804     c                   end
108000050523     c                   if        v2ctpm = ' '
108100050523     c                   eval      v2ctpm = 'P'
108200050523     c                   end
108300160426     c                   if        *in54
108400160426     C                   MOVEL     §dstturno     v2ctur                         *MATT/POM
108500160426     c                   else
108600160426     C                   clear                   v2ctur                         *MATT/POM
108700160426     c                   end
108800160427     c                   if        v2ctur = ' ' and dsttpm = ' '
108900160426     c                   eval      v2ctur = '1'
109000160426     c                   end
109100061228     C                   MOVEL     dstFca        V2CFca                         *MATT/POM
109200070719     C                   MOVEL     dstpda        V2Cpda                         *MATT/POM
109300080207     C                   MOVEL     §dsttstpda    V2Ctstpda                      *MATT/POM
109400061228     C                   MOVEL     dstFPP        V2CFPP                         *MATT/POM
109500060929     C                   MOVEL     dstPEP        V2CPEP                         *PIKING(S/N)
109600041005     C                   MOVEL     qdtnbnaf      V2CNBN                         *NR.BANCALI
109700050120     C                   MOVEL     qdtfnbaf      V2CfNBN                        *NR.BANCALI
109800061228     c                   eval      v2cnot  = %trimr(dstnot)
109900070726     c                   if        v2cpda = ' '
110000070726     c                   eval      v2cpda = 'N'
110100070726     c                   end
110200950525    1C                   ENDIF
110300101022     c* controllo se aut ha o meno il pda
110400080212     c                   clear                   msgv
110500080212     c                   if        *in96
110600101022     c                   exsr      srctrpda
110700080319     C                   ENDIF
110800950525     C*
110900170206      ***----
111000170206      *** Controlla Disaccreditamento Autista
111100170206      **
111200170207     c                   clear                   HPdrForza         7 0
111300170206      ***----
111400170206     C*
111500950524     C                   ENDSR
111600061002     C**********************************************************************
111700950524     C*--- ANNULLAMENTO FOLGIO DISTINTA ------------------------------*
111800061002     C**********************************************************************
111900950524     C     ANNDIS        BEGSR
112000061002     c                   do
112100061002     C* NON E' POSSIBILE ANNULLARE  DISTINTE CONSEGNE CHE HANNO
112200061002     C* BOLLE O SPUNTE SOPRA
112300960904    1C     WSPU          IFEQ      'S'
112400950525     C     WBOL          OREQ      'S'
112500950525     C                   MOVEL     MSG(14)       V2CMSG
112600950525     C                   SETON                                        28
112700061002     c                   leave
112800061002   X1C                   End
112900010314     C* WORM = "S"  --->  ESISTONO O.R.M. IN FASE < 900
113000010314   1AC     WORM          IFEQ      'S'
113100010314     C                   MOVEL     MSG(21)       V2CMSG
113200010314     C                   SETON                                        28
113300061002     c                   leave
113400061002  X1AC                   End
113500061002     C* ANNULLO IL RECORD IN FiDST00F
113600061228     c                   z-add     4             wnpg
113700070410     C     KDST          chain     fidst09l
113800070410     c                   if        %found(fidst09l)
113900061228     C                   MOVEL     'A'           DSTATB
114000061228     C                   UPDATE    FiDST000
114100061228     c                   end
114200061222     C* ANNULLO I RECORD IN FiDST30F
114300061222     C     KDST1         setll     FiDST31l
114400061222     c                   do        *hival
114500061222     C     KDST1         reade     FiDST31l
114600061222     c                   if        %eof(fidst31l)
114700061222     c                   leave
114800061222     c                   end
114900061222     C                   MOVEL     'A'           DST3ATB
115000061222     C                   UPDATE    FiDST300
115100061222     c                   enddo
115200070717     C* ANNULLO I RECORD IN FiDST50F
115300070726     c                   if        §OGDDAO <> ' '
115400070717     C     KDST1         setll     FiDST51l
115500070717     c                   do        *hival
115600070717     C     KDST1         reade     FiDST51l
115700070717     c                   if        %eof(fidst51l)
115800070717     c                   leave
115900070717     c                   end
116000070717     C                   MOVEL     'A'           DST5ATB
116100070717     C                   UPDATE    FiDST500
116200070717     c                   enddo
116300070726     c                   end
116400041005     C* ANNULLO IL RECORD IN Fiqdt01l
116500070313     c                   if        comdda <> 0
116600061002     C     Kqdt          chain     Fiqdt01l
116700061002     c                   if        %found(fiqdt01l)
116800041005     C                   movel     '*'           qdtatb
116900061002     C                   update    fiqdt000
117000070109     c                   end
117100061002    1C                   ENDIF
117200061002     c                   enddo
117300950524     C*
117400950524     C                   ENDSR
117500061002     C**********************************************************************
117600170207     C*--- CONTROLLO AUTISTA ACCREDITATO ----------------------------*
117700170207     C**********************************************************************
117800170207     C     CTRACCR       BEGSR
117900170207     C*
118000170207      *** Controlla Disaccreditamento Autista
118100170207    2C     V2Cpdr        IFNE      *zeros
118200170207    2C     V2Cpdr        andne     HPdrForza
118300170207     c                   clear                   fnlr50wds
118400170207     C                   eval      savpjbu = kpjbu
118500170207     c                   z-add     v2cpdr        §50autista
118600170207     c                   z-add     wdfv          §50datadis
118700170207     c                   eval      §50WINDOW  = 'F'
118800170207     C                   eval         kpjbu = fnlr50wds
118900170207      ***
119000170207     C                   call      'FNLR50WR'
119100170207     C                   parm                    kpjba
119200170207     C                   eval      fnlr50wds = kpjbu
119300170207     C                   eval      kpjbu = savpjbu
119400170207      ***  Se Forzato
119500170207     c                   IF        §50esito ='1'
119600170207     c                   IF        §50funz  ='F07'
119700170207     c                   eval      HPdrForza = V2Cpdr
119800170207     c                   END
119900170207     c                   IF        §50funz  ='F12'
120000170208     c                   eval      HPdrForza = *all'9'
120100170207     C                   MOVEL     MSG(30)       V2CMSG
120200170208     C                   SETON                                        44
120300170207     c                   END
120400170207    1C                   ENDIF
120500170207      ***
120600170207    1C                   ENDIF
120700170207      ***
120800170207     C                   ENDSR
120900170207     C**********************************************************************
121000950607     C*--- CONTROLLO DATA DISTINTA ----------------------------------*
121100061002     C**********************************************************************
121200950607     C     CTRDDC        BEGSR
121300061002     c                   do
121400950607     C* DATA DISTINTA OBBLIGATORIA
121500950607     C                   Z-ADD     V2CDFV        G02DAT
121600950607     C                   MOVEL     *BLANK        G02ERR
121700950607     C                   CALL      'XSRDA8'
121800950607     C                   PARM                    WLBDAT
121900950607    1C     G02ERR        IFEQ      '1'
122000950607     C                   MOVEL     MSG(15)       V2CMSG
122100950607     C                   SETON                                        4528
122200061002     C                   leave
122300950607    1C                   ENDIF
122400950607     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6 (ANCHE NEL SUBFILE)
122500061002     C                   Z-ADD     G02INV        WDFV
122600950607     C                   Z-ADD     G02DAT        V2CDFV
122700950607     C   03              Z-ADD     G02DAT        VSCDFV
122800950607     C* LA DATA NON PUO' ESSERE: - INFERIORE A UDATE
122900950607     C*                          - SUPERIORE A UDATE DI 3 GIORNI
123000950613    0C     *IN15         IFEQ      *OFF
123100950613     C     *IN06         ANDEQ     *OFF
123200950613     C*
123300950607    1C     WDFV          IFLT      DATEU8
123400950607     C                   MOVEL     MSG(16)       V2CMSG
123500950607     C                   SETON                                        4528
123600061002     C                   leave
123700950607    1C                   ENDIF
123800020419      *controllo range fra le date di oggi e quella immessa
123900020419     c                   eval      dadata = dateu8
124000020419     c                   eval      adata  = wdfv
124100020419     c                   CALL      'XSRLAV8'
124200020419     c                   PARM                    Wdata8
124300030917     c                   if        giolav = 0
124400030917     c                   clear                   giorni
124500030917     c                   move      dateu8        wdtiso1
124600030917     c                   move      wdfv          wdtiso2
124700030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
124800030917     c                   if        giorni > 30
124900030917     c                   SETON                                        4528
125000030917     c                   MOVEL     msg(16)       v2cmsg
125100061002     C                   leave
125200030917     c                   endif
125300030917     c                   endif
125400020419     c                   if        giolav > §dgddc
125500020419     c                   SETON                                        4528
125600020419     c                   MOVEL     msg(16)       v2cmsg
125700061002     C                   leave
125800020419     c                   end
125900950607     C* CONTROLLO CHE NON ESISTANO CONTEGGI GIA' CONFERMATI NELLA
126000061002     C* DATA IMMESSA
126100011121     C     KFTT          SETLL     FIFTT02L                               32
126200011121     C  N32KFTT          SETLL     FIFST000                               32
126300950607    1C     *IN32         IFEQ      *ON
126400950607     C                   MOVEL     MSG(17)       V2CMSG
126500950607     C                   SETON                                        4528
126600061002     C                   leave
126700950607    1C                   ENDIF
126800950607     C*
126900950607     C* SE LA DATA E' STATA MODIFICATA CONTROLLO SE ESISTE GIA'
127000061002     C* UNA DISTINTA APERTA PER QUEL PADRONCINO
127100950613    1C     VHCDFV        IFNE      V2CDFV
127200950607     C                   Z-ADD     WDFV          WDFV2
127300950607     C                   Z-ADD     V2CDFV        VHCDFV
127400061002     C                   EXSR      LEGdst
127500061011     c                   end
127600950607     C     V2CPDR        LOOKUP    PDR                                    10
127700950607     C   10              MOVEL     'DS'          VSCDSN
127800950607     C  N10              CLEAR                   VSCDSN
127900061002     c*
128000061011   X1C*                  ELSE
128100061011    2C*    VSCDSN        IFEQ      'DS'
128200061011     C*                  SETON                                        10
128300061011     C*                  ELSE
128400061011     C*                  SETOFF                                       10
128500061011    2C*                  ENDIF
128600061011    1C*                  ENDIF
128700950613    0C                   ENDIF
128800950607     C*
128900061002     c                   enddo
129000061002     C*
129100061002     C                   ENDSR
129200061002     C**********************************************************************
129300950525     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
129400061002     C**********************************************************************
129500950525     C     CTRD02        BEGSR
129600061002     c                   do
129700170208     C* CONTROLLO ACCREDITAMENTO.
129800170208     C                   EXSR      CTRACCR
129900061002     C* DATA DISTINTA
130000950613     C                   EXSR      CTRDDC
130100061002     C   28              leave
130200170206      ***
130300061002     C* NUMERO BOLLE E NUMERO BANCALI
130400950529     C* CONTROLLO CHE NON VENGANO INSERITI VALORI NEGATIVI
130500950529     C                   MLLZO     1             V2CNBN
130600110329     c*                  if        v2cnbn > 0
130700050120     c                   move      'S'           v2cfnbn
130800110329     c*                  endif
130900061227     C* PRESTAZIONE PADRONCINO (MATTINO/POMERIGGIO/giornata)
131000070111    1C     V2CFPP        IFEQ      ' '
131100070111     C                   MOVEL     MSG(19)       V2CMSG
131200070111     C                   SETON                                        4728
131300070111     C                   leave
131400070111    1C                   ENDIF
131500061002     C* PIKING (SI/NO)
131600950526    1C     V2CPEP        IFEQ      ' '
131700950526     C                   MOVEL     MSG(19)       V2CMSG
131800950526     C                   SETON                                        4828
131900950526    1C                   ENDIF
132000070108     C* carico (SI/NO)
132100070108    1C     V2Cfca        IFEQ      ' '
132200070108     C                   MOVEL     MSG(19)       V2CMSG
132300070108     C                   SETON                                        4828
132400070108    1C                   ENDIF
132500080212     c* controllo flag dati per PDA se attivo
132600080212     c                   if        *in96
132700080208     C                   if        (v2cpda = 'O' and
132800080208     c                             §OGPDAORM = ' ') or
132900080208     C                             (v2cpda = 'C' and
133000080208     c                             §OGPDACON = ' ') or
133100080208     C                             (v2cpda = 'E' and
133200080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
133300080208     C                   MOVEL     MSG(26)       V2CMSG
133400080208     c                   select
133500080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
133600080208     c                   eval      v2cmsg = %trim(v2cmsg)+' CONSEGNE'
133700080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
133800080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' ORM'
133900080208     c                   endsl
134000080208     C                   SETON                                        5128
134100080208     C                   leave
134200080208     C                   ENDIF
134300080319     c* se distinta già preparata per pda non posso cambiare il flag x con.
134400080319     c* se ho la fase PDC
134500081110     c                   if        (dstpda = 'E' or dstpda ='C') and
134600081110     c                             (v2cpda ='N' or v2cpda='O') and
134700080319     c                             fasedst ='PDC'
134800080319     c                   seton                                        5128
134900080319     C                   MOVEL     msg(28)       V2CMSG
135000080319     c                   end
135100080319     c* se distinta già preparata per pda non posso cambiare il flag x orm
135200080319     c* se ho il FIPCO00T
135300081110     c                   if        (dstpda = 'E' or dstpda ='O') and
135400081110     c                             (v2cpda = 'N' or v2cpda ='C') and
135500080320     c                              faseorm ='PDR'
135600080319     c                   seton                                        5128
135700080319     C                   MOVEL     msg(28)       V2CMSG
135800080319     c                   end
135900080208     c* controllo flag test per PDA
136000080208     C                   if        (v2ctstpda = 'O' and
136100080208     c                             §OGPDAORM = ' ') or
136200080208     C                             (v2ctstpda = 'C' and
136300080208     c                             §OGPDACON = ' ') or
136400080208     C                             (v2ctstpda = 'E' and
136500080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
136600080208     C                   MOVEL     MSG(26)       V2CMSG
136700080208     c                   select
136800080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
136900080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' CONSEGNE'
137000080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
137100080208     c                   eval      v2cmsg = %trim(v2cmsg) + ' ORM'
137200080208     c                   endsl
137300080208     C                   SETON                                        5228
137400080208     C                   leave
137500080208     C                   ENDIF
137600080215     c* controllo la congruenza dei valori impostati nel test x pda e
137700080215     c* e nei dati x pda
137800080215     C                   if        v2ctstpda <>' '
137900080215     C                   if        (v2cpda = 'O' and
138000080215     C                             v2ctstpda <>'O') or
138100080215     C                             (v2cpda = 'C' and
138200080215     C                             v2ctstpda <>'C') or
138300080215     C                             v2cpda = 'N'
138400080208     C                   MOVEL     MSG(27)       V2CMSG
138500080208     C                   SETON                                        5228
138600080208     C                   leave
138700080208     C                   ENDIF
138800110223     c* si può inserire il flag di test C/E solo se non si sono ricevuti
138900110223     c* esiti consegne (se già inseriti ma non ancora ricevuti, paziena !!!)
139000110223     c                   if        §dsttstpda = ' ' and
139100110223     c                             v2ctstpda <> 'O'
139200110223     c     kpct          setll     fipct02l                               52
139300110223     c                   if        *in52
139400110223     C                   MOVEL     MSG(28)       V2CMSG
139500110223     C                   SETON                                        28
139600110223     C                   leave
139700110223     c                   end
139800110223     c                   end
139900110224     c* se si era in test con Entrambi e si vuole passare in buona con uno
140000110224     c* dei due, non si può fare
140100110530     c*                  if        §dsttstpda = 'E' and
140200110530     c*                            v2ctstpda <> 'E'
140300110530     C*                  MOVEL     MSG(27)       V2CMSG
140400110530     C*                  SETON                                        2852
140500110530     C*                  leave
140600110530     c*                  end
140700110223     c* non è possibile eliminare il flag di test
140800110223     c                   else
140900110223     c                   if        §dsttstpda <> ' ' and
141000110223     c                             (faseorm ='PDR' or fasedst = 'PDC')
141100110223     C                   MOVEL     MSG(28)       V2CMSG
141200110223     C                   SETON                                        5228
141300110223     C                   leave
141400110223     c                   end
141500080215     C                   ENDIF
141600101022     c* controllo se aut ha o no il pda
141700101022     c                   exsr      srctrpda
141800080212     C                   ENDIF
141900160426     c* controllo turno solo per parcel
142000160427     C                   if        v2ctpm = 'M' and v2ctur <> ' '
142100160503     c                             and *in54
142200160426     C                   MOVEL     MSG(29)       V2CMSG
142300160426     C                   SETON                                        5528
142400160426     C                   leave
142500160426     C                   ENDIF
142600160429     C                   if        v2ctpm = 'P' and v2ctur = ' '
142700160503     c                             and *in54
142800160429     C                   MOVEL     MSG(19)       V2CMSG
142900160429     C                   SETON                                        5528
143000160429     C                   leave
143100160429     C                   ENDIF
143200950526     C*
143300170208     C*
143400061002     c                   enddo
143500061002     C*
143600061002     C                   ENDSR
143700101022     c**********************************************************************
143800101022     c     srctrpda      begsr
143900101022     c**********************************************************************
144000101022     c* se procedura attiva e aut. in test avviso
144100101022     c                   clear                   msgv
144200101022     C                   if        (v2ctstpda ='E' and
144300101022     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
144400101022     C                             or (v2ctstpda ='C' and
144500101022     C                             §OGPDACON='S')
144600101022     C                             or (v2ctstpda ='O' and
144700101022     C                             §OGPDAORM='S')
144800101022     C                   eval      msgv= 'ATT. gestione PDA attivata'
144900101022     C                   ENDIF
145000101022     c* se aut. ha dati per pda differenti da quelli della distinta avviso
145100101022     c                   clear                   msgv1
145200101022     C*                  if        (v2ctstpda =' ' and
145300101022     C*                            v2ctstpda <> §apdtstpda) or
145400101022     C                   if        (v2cpda = 'N' and
145500101022     C                             v2cpda <> §apdpda)
145600101022     C                   eval      msgv1= 'ATT. Aut. con PDA'
145700101022     c                   end
145800101022     C*                  if        (v2ctstpda <>' ' and
145900101022     C*                            §apdtstpda =' ') or
146000101022     C                   if        (v2cpda <> 'N' and
146100101022     C                             §apdpda = 'N')
146200101022     C                   eval      msgv1= 'ATT. Aut. senza PDA'
146300101022     c                   end
146400101022     c*
146500101022     C                   ENDSR
146600061002     C**********************************************************************
146700950525     C*--- AGGIORNAMENTO ---------------------------------------------*
146800061002     C**********************************************************************
146900950525     C     AGGD02        BEGSR
147000061227     c                   move      wdfv          dtaiso
147100061227     c                   move      dtaiso        comiso            8 0
147200061222     c                   do
147300950703     C* SE SONO IN IMMISSIONE PRELEVO NUMERATORE FOGLIO DISTINTA
147400070108    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
147500070108     C     *IN03         OREQ      *ON                                          *CMD15
147600070122     C                   EXSR      CRTNFV
147700070122     c                   eval      wnfv = v2cnfv
147800070717     c*
147900070717     c                   clear                   fiapd2ds
148000070515     c* se inserimento multiplo non propone i giri li prende dall'anagrafica
148100070720     c                   if        *inkf
148200070515     c     kapd21        setgt     fiapd22l
148300070515     c     kapd2         readpe    fiapd22l
148400070717     c                   if        (not %eof(fiapd22l) and apd2atb = ' ' and
148500070717     c                             (comiso<apd2dde or comiso>apd2dscad)) or
148600070717     c                             %eof(fiapd22l)
148700070717     c                   clear                   fiapd2ds
148800070717     c                   end
148900070717     c* se passa dalla singola videata propone i giri dell'anagrafica
149000070515     c                   else
149100070515     c                   exsr      sraltro
149200070515     c                   eval      *inkb = '0'
149300070720     c                   if        d02cmde ='KF'
149400070720     c                   eval      *inkf = '1'
149500070720     c                   else
149600070720     c                   seton                                        28
149700070720     C                   MOVEL     MSG(25)       V2CMSG
149800070720     c                   leave
149900070720     c                   end
150000070720     c                   end
150100061011    1C                   ENDIF
150200061002     C* F I D S T 0 0 F
150300061002     c                   clear                   fidstds
150400070410     C     KDST          CHAIN     fidst09l                           3039
150500061228     C* 39 ON - RECORD ALLOCATO
150600061228     C     *IN39         IFEQ      *ON
150700061228     C                   SETON                                        28
150800061228     C                   MOVEL     MSG(20)       V2CMSG
150900061228     C                   leave
151000061228     C                   ENDIF
151100070717     c*allineao i giri se variata la data distinta
151200070410     c                   if        %found(fidst09l) and wdfv <> dstdfv
151201170920      *salvo data per aggiornamento fiqdt
151202170920     c                   movel     dstdfv        wdfvex
151300070717     c* consegne
151400061228     c     kdst1         setll     fidst31l
151500061228     c                   do        *hival
151600061228     c     kdst1         reade     fidst31l
151700061228     c                   if        %eof(fidst31l)
151800061228     c                   leave
151900061228     c                   end
152000061228     C                   MOVEL     wdfv          dst3DFV
152100061228     c                   update    fidst300
152200061228     c                   enddo
152300070717     c* ORM
152400070726     c                   if        §OGDDAO <> ' '
152500070717     c     kdst1         setll     fidst51l
152600070717     c                   do        *hival
152700070717     c     kdst1         reade     fidst51l
152800070717     c                   if        %eof(fidst51l)
152900070717     c                   leave
153000070717     c                   end
153100070717     C                   MOVEL     wdfv          dst5DFV
153200070717     c                   update    fidst500
153300070717     c                   enddo
153400061228     c                   end
153500070726     c                   end
153600061228     c*
153700950525     C                   CLEAR                   DSTATB
153800061002     C                   MOVEL     V2CFGS        dstFGS
153900950525     C                   MOVEL     V2CPDR        DSTPDR
154000070122     C                   MOVEL     'A'           DSTtipa
154100061222     C                   MOVEL     wnpg          DSTnpg
154200061228     c                   eval      dstnot  = %trimr(v2cnot)
154300061006     C                   eval      DSTNfv = v2cnfv
154400061006     C                   MOVEL     wdfv          dstDFV
154500061228     C                   MOVEL     V2CFca        dstFca
154600070717     C                   MOVEL     V2Cpda        dstpda
154700080207     C                   MOVEL     V2Ctstpda     §dsttstpda
154800061228     C                   MOVEL     V2CFPP        dstFPP
154900061002     C                   MOVEL     V2CPEP        dstpEP
155000061002     c                   movel     v2ctpm        dsttpm
155100050523     c                   if        v2ctpm = 'P'
155200061002     c                   clear                   dsttpm
155300050523     c                   end
155400160426     c                   movel     v2ctur        §dstturno
155500160426     c                   if        v2ctur = '1'
155600160426     c                   clear                   §dstturno
155700160426     c                   end
155800080207     c                   movel     ddstflr       dstflr
155900070410     c                   if        %found(fidst09l)
156000061002     C                   update    Fidst000
156100061228     c                   else
156200061228     c* INSERIMENTO
156300070717     c* consegne
156400061002     c                   movea     tc1           tc1d
156500061002     c                   movea     ts1           ts1d
156600070123     c                   if        apd2ftc = ' '
156700070123     c                   eval      dstftc = 'E'
156800070123     c                   else
156900061227     c                   eval      dstftc = apd2ftc
157000070123     c                   end
157100070123     c                   if        apd2fts = ' '
157200070123     c                   eval      dstfts = 'E'
157300070123     c                   else
157400070123     c                   eval      dstfts = apd2fts
157500070123     c                   end
157600061227     c                   eval      dstvlu = apd2vlu
157700061222     c                   eval      dstpkl = apd2pkl
157800070222     c                   eval      dstnbl = apd2nbl
157900070122     c                   eval      dstnftl= apd2nftl
158000061002     c                   eval      dsttol = apd2tol
158100070717     c                   if        apd2inc = ' '
158200070123     c                   eval      dstinc = 'S'
158300070123     c                   else
158400070123     c                   eval      dstinc = apd2inc
158500070123     c                   end
158600070123     c                   eval      DSTPKLA=0
158700070123     c                   eval      DSTVLUA=0
158800070123     c                   eval      DSTSTPA=0
158900070222     c                   eval      DSTnblA=0
159000070726     c* ORM attivita
159100070726     c                   if        §OGDDAO <> ' '
159200070717     c                   eval      dstrvlu = apd2rvlu
159300070717     c                   eval      dstrpkl = apd2rpkl
159400070717     c                   eval      dstrnbl = apd2rnbl
159500070717     c                   eval      dstrnftl= apd2rnftl
159600070717     c                   eval      dstrtol = apd2rtol
159700070717     c                   if        apd2rinc = ' '
159800070717     c                   eval      dstrinc = 'S'
159900070717     c                   else
160000070717     c                   eval      dstrinc = apd2rinc
160100070717     c                   end
160200070717     c                   if        apd2rspi = ' '
160300070717     C*???
160400070717     c                   eval      dstrspi = 'N'
160500070717     c                   else
160600070717     c                   eval      dstrspi = apd2rspi
160700070717     c                   end
160800070726     c                   else
160900070726     c                   clear                   dstrvlu
161000070726     c                   clear                   dstrpkl
161100070726     c                   clear                   dstrnbl
161200070726     c                   clear                   dstrnftl
161300070726     c                   clear                   dstrtol
161400070726     c                   clear                   dstrinc
161500070726     c                   clear                   dstrspi
161600070726     c                   end
161700070717     c                   eval      DSTrPKLA=0
161800070717     c                   eval      DSTrVLUA=0
161900070717     c                   eval      DSTrSTPA=0
162000070717     c                   eval      DSTrnblA=0
162100070717     c                   eval      DSTrnbaA=0
162200061002     C                   WRITE     Fidst000
162300070717     c* scrivo i giri consegne
162400061222     c                   clear                   fidst300
162500061222     c                   eval      dst3fgs = dstfgs
162600061227     c                   eval      dst3npg = wnpg
162700061222     c                   eval      dst3dfv = dstdfv
162800061222     c                   eval      dst3nfv = dstnfv
162900070122     c     apd2nras      setll     fiapd31l
163000061222     c                   do        *hival
163100070122     c     apd2nras      reade     fiapd31l
163200061222     c                   if        %eof(fiapd31l)
163300061227     c                   leave
163400061227     c                   end
163500070720     c                   if        apd3atb <> ' '
163600070720     c                   iter
163700070720     c                   end
163800070129     c                   eval      dst3fgsg= apd3fgsg
163900070129     c                   eval      dst3cgi = apd3cgi
164000070122     c                   eval      dst3prgg= apd3prgg
164100061222     c                   eval      dst3sql = apd3sql
164200070723     c                   clear                   fidg09ds
164300070723     c                   eval      d09ifgs = apd3fgsg
164400070723     c                   eval      d09icgi = apd3cgi
164500070723     c                   eval      D09ITUG = 'C'
164600070717     c                   exsr      srcgi
164700070717     c                   if        d09oerr <>'1'
164800061222     c                   write     fidst300
164900070717     c                   end
165000061222     c                   enddo
165100070717     c* scrivo i giri ORM
165200070726     c                   if        §OGDDAO <> ' '
165300070717     c                   clear                   fidst500
165400070717     c                   eval      dst5fgs = dstfgs
165500070717     c                   eval      dst5npg = wnpg
165600070717     c                   eval      dst5dfv = dstdfv
165700070717     c                   eval      dst5nfv = dstnfv
165800070717     c     apd2nras      setll     fiapd51l
165900070717     c                   do        *hival
166000070717     c     apd2nras      reade     fiapd51l
166100070717     c                   if        %eof(fiapd51l)
166200070717     c                   leave
166300070717     c                   end
166400070720     c                   if        apd5atb <> ' '
166500070720     c                   iter
166600070720     c                   end
166700070717     c                   eval      dst5fgsg= apd5fgsg
166800070717     c                   eval      dst5cgi = apd5cgi
166900070717     c                   eval      dst5prgg= apd5prgg
167000070717     c                   eval      dst5sql = apd5sql
167100070723     c                   clear                   fidg09ds
167200070723     c                   eval      d09ifgs = apd5fgsg
167300070723     c                   eval      d09icgi = apd5cgi
167400070723     c                   eval      D09ITUG = 'R'
167500070717     c                   exsr      srcgi
167600070717     c                   if        d09oerr <>'1'
167700070717     c                   write     fidst500
167800070717     c                   end
167900070717     c                   enddo
168000070122     c                   end
168100070726     c                   end
168200070122     C* F I Q D T 0 0 F
168300070313     c                   if        comdda <> 0
168400070122     C     Kqdt          chain     fiqdt01l                           27
168500070122     c                   if        not *in27
168600070122     C                   clear                   qdtatb
168700070122     c                   if        v2cnbn <> qdtnbnaf
168800070122     c                   z-add     v2cnbn        qdtnbnaf
168900070122     c                   z-add     hhmm          qdtoraaf
169000070122     c                   z-add     dateu8        qdtdaiaf
169100070122     c                   move      v2cfnbn       qdtfnbaf
169200070122     C                   endif
169300100318     c                   eval      qdtddc = dstdfv
169400140210     C                   z-add     dstpdr        qdtpdr
169500070122     C                   update    fiqdt000
169600070122     c                   else
169700070122      *SCRITTURA testata quadrature distinte orm RIENTRI
169800070122     c                   clear                   fiqdt000
169900070122     C                   z-add     dstfgs        qdtfgs
170000070122     C                   z-add     dstnfv        qdtnfv
170100070122     C                   z-add     dstdfv        qdtddc
170200070122     C                   z-add     dstpdr        qdtpdr
170300110329     c     kapdd         chain     fiapd01l
170400110329     c                   if        %found(fiapd01l) and apdptp <> *blank
170500110329     c                   move      apdptp        v2cnbn
170600110329     c                   endif
170700110329     c*                  if        v2cnbn > 0
170800070122     c                   z-add     v2cnbn        qdtnbnaf
170900070122     c                   z-add     hhmm          qdtoraaf
171000070122     c                   z-add     dateu8        qdtdaiaf
171100110329     c                   move      'S'           qdtfnbaf
171200110329     C*                  endif
171300070122     C                   WRITE     Fiqdt000
171400070122     c                   end
171500070122     c                   end
171600061222     c                   enddo
171700061002     c*
171800061002     C                   ENDSR
171900061002     C**********************************************************************
172000061229     C* aggiorno FNFVV e FNDST
172100061002     C**********************************************************************
172200061229     C     sraggold      BEGSR
172300070313     c                   if        comdda <> 0
172400061229     C                   eval      d05fgs = V1CFGS
172500061229     c                   eval      d05com= 'N'
172600061229     c                   eval      d05dfv= wdfv
172700061229     c                   eval      d05nfv= wnfv
172800061229     c                   clear                   d05esi
172900061229     c                   eval      kpjbu=fidg05ds
173000061229     c                   call      'FIDG05R'
173100061229     c                   parm                    kpjba
173200061229     c                   eval      fidg05ds = kpjbu
173300061229     c                   if        d05esi <> ' '
173400070315     c                   rolbk
173500061229     c                   eval      v2cmsg = msg(25)
173600061229     c                   seton                                        28
173700061229     c                   end
173800070109     c                   end
173900061229     C                   ENDSR
174000061229     C**********************************************************************
174100061229     C*--- GESTIONE VIDEATA SUBFILE ----------------------------------*
174200061229     C**********************************************************************
174300061229     C     GESS03        BEGSR
174400950525     C* PULIZIA SUBFILE
174500950525     C                   SETON                                        21
174600950525     C                   WRITE     LRF0C03
174700950525     C                   SETOFF                                       21
174800950525     C* CARICAMENTO SUBFILE
174900950525     C                   EXSR      RIES03
175000950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
175100950525     C     NRR           IFEQ      0
175200950525     C                   MOVEL     'F'           WGES
175300061002     C                   else
175400950525     C* EMISSIONE FORMATO
175500061002     C                   do        *hival
175600070207     C                   WRITE     LRF0Z04
175700950525     C                   EXFMT     LRF0C03
175800950525     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
175900950525     C                   CLEAR                   V2CMSG
176000950525     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
176100950525     C                   SETOFF                                       284546
176200041008     C                   SETOFF                                       474849
176300160426     C                   SETOFF                                       080955
176400170207     C                   SETOFF                                       44
176500160426      **
176600160426     C* F11 = Cambia ordinamento
176700160426if  1C                   IF        *INKK
176800160426     C                   EXSR      Ordina
176900160426     C                   iter
177000160426e   1C                   ENDIF
177100950607     C** F3 - FINE
177200950607    1C     *INKC         IFEQ      *ON
177300950607     C                   MOVEL     'F'           WGES
177400950525     C                   CLEAR                   V1CNFP                         *DIST.PRECEDENTE
177500061002     C                   leave
177600950525    1C                   ENDIF
177700950607     C** F12 - RITORNO
177800950607    1C     *INKL         IFEQ      *ON
177900950607     C                   MOVEL     '1'           WGES
178000061006     C                   rolbk
178100061002     C                   leave
178200950607    1C                   ENDIF
178300950526     C* 08 ON  - E' STATO PREMUTO IL CMD1-CONFERMA PER DISTINTA
178400950526     C   KA              SETON                                        08
178500950526     C* 09 ON  - E' STATO PREMUTO IL CMD6-CONFERMA GLOBALE
178600950526     C   KF              SETON                                        09
178700950525     C* CONTROLLO DATI SUBFILE
178800950525     C                   EXSR      CTRS03
178900061002     C   28              iter
179000950607     C** F6  - AGGIORNA
179100950525     C   KF
179200950525     COR KA              EXSR      AGGS03
179300950525     C** ENTER - RIEMETTE LA VIDEATA
179400061011     c                   if        (not *in08 and not *in09) or *in28
179500061011     C                   iter
179600061011     c                   else
179700061011     c                   leave
179800061011     c                   end
179900061002     c                   enddo
180000950526     C* RITORNO ALLA VIDEATA INIZIALE (QUELLA DELLE SELEZIONI)
180100950525     C                   MOVEL     '1'           WGES
180200061002     c                   end
180300950525     C     ENDGS3        ENDSR
180400160426      *-------------------------------------------------------------------------
180500160426      * TASTI FUNZIONALI di ordinamento
180600160426      *-------------------------------------------------------------------------
180700160426     C     Combinazioni  BEGSR
180800160426      **    F11
180900160426     c                   if        ordinamento = '1'
181000160426     c                   eval      ordinamento = '2'
181100160426      **
181200160505     c                   elseIF    ordinamento = '2' and *in54
181300160426     c                   eval      ordinamento = '3'
181400160427      **
181500160505     c                   elseIF    ordinamento = '3' or
181600160505     c                             ordinamento = '2'
181700160427     c                   eval      ordinamento = '1'
181800160426     c                   end
181900160427     c                   if        Ordinamento = '1'
182000160427     c                   eval      vcmd = 'F11=Rag.Soc.'
182100160427      **
182200160505     c                   elseif    Ordinamento = '2' and *in54
182300160427     c                   eval      vcmd = 'F11=Turno/Aut.'
182400160427     c**
182500160505     c                   elseif    Ordinamento = '3' or
182600160505     c                             Ordinamento = '2'
182700160427     c                   eval      vcmd = 'F11=Aut.'
182800160427     c                   end
182900160426      *
183000160426     C                   ENDSR
183100160426      *-------------------------------------------------------------------------
183200160426      * Ordina - TASTO FUNZIONE F11 --> Ragione sociale/cod. aut.
183300160426      *-------------------------------------------------------------------------
183400160426     C     Ordina        BEGSR
183500160426      *
183600160426     c                   exsr      COMBINAZIONI
183700160428      * IMPOSTO ENTRATA AD 'A'
183800160428     C                   EVAL      nrr  = 1
183900160426      **
184000160426     C                   CLEAR                   QLGSCB
184100160426     C                   CLEAR                   QLGSCB00
184200160426      *
184300160426     ** 2 campi chiave
184400160428     C                   EVAL      QLGNBRK    = 1
184500160427     c                   if        ordinamento = '3'
184600160428     C                   EVAL      QLGNBRK    = 2
184700160427     c                   end
184800160427     **
184900160428     C                   EVAL      QLGSO      = Ascendente
185000160426
185100160427     c                   if        ordinamento = '1' or
185200160427     c                             ordinamento = '3'
185300160428     ** il codice autista
185400160428     C                   EVAL      QLGSP      = 1
185500160426     C                   EVAL      QLGSS      = %SIZE(vscpdr)
185600160426     C                   EVAL      QLGDT      = Numerico
185700160428     c* se ordino per codice lo metto al primo campo chiave
185800160428     c                   if        ordinamento = '1'
185900160428     C                   EVAL      QLGKL(1)   = QLGSKL
186000160428     c                   else
186100160428     c* se ordino per turno/codice lo metto al secondo campo chiave
186200160428     C                   EVAL      QLGKL(2)   = QLGSKL
186300160428     c                   end
186400160426      *
186500160426     c                   elseIF    ordinamento = '2'
186600160426     ** La ragione sociale
186700160428     C                   EVAL      QLGSP      = 1 + %SIZE(vscpdr)
186800160427     C                   EVAL      QLGSS      = %SIZE(VSDPDR)
186900160426     C                   EVAL      QLGDT      = Carattere
187000160428     C                   EVAL      QLGKL(1)   = QLGSKL
187100160428     c                   end
187200160428     c                   if        ordinamento = '3'
187300160426     ** turno e codice
187400160428     C                   EVAL      QLGSP      = 1 + %SIZE(vscpdr) +
187500160428     c                                          %SIZE(VSDPDR)
187600160428     C                   EVAL      QLGSS      = %SIZE(vsctur)
187700160426     C                   EVAL      QLGDT      = Carattere
187800160428     C                   EVAL      QLGKL(1)   = QLGSKL
187900160426     C                   ENDIF
188000160426
188100160426
188200160426      * Load other sort parameters.
188300160426     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
188400160426     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
188500160426     C                   EVAL      QLGRT     = 8
188600160426     C                   EVAL      QLGOKL    = 80
188700160426     C                   EVAL      QLGLKE    = 16
188800160426     C                   EVAL      QLGLSS    = 290
188900160426
189000160426      * Initialize Sort I/O API fields.
189100160426     C                   EVAL      QLGRL00  = QLGRL
189200160426     C                   EVAL      QLGRC00  = 1
189300160426     C                   CLEAR                   QUSEI
189400160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
189500160426
189600160426      * First step - Initialize the sort routine.
189700160426     C                   CALL      'QLGSORT'
189800160426     C                   PARM                    QLGSCB
189900160426     C                   PARM                    NotUsed
190000160426     C                   PARM                    NotUsed
190100160426     C                   PARM                    SizeList
190200160426     C                   PARM                    ReturnSize
190300160426     C                   PARM                    QUSEC
190400160426
190500160426      * Next step - Write records to I/O routine.
190600160426     C                   EVAL      QLGRT00 = Put
190700160426
190800160426     C                   DO        STMREC1       x                 4 0
190900160426
191000160427     C     x             CHAIN     LRF0S03
191100160426
191200160426     ** Solo le righe con Selected = 'Y' sono riordinate,
191300160426     ** quindi per fare un ordinamento di tutte le righe
191400160426     ** metto 'Y' sempre.
191500160426     C                   EVAL      Selected = 'Y'
191600160426
191700160426     C                   CLEAR                   QUSEI
191800160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
191900160426     C                   CALL      'QLGSRTIO'
192000160426     C                   PARM                    QLGSCB00
192100160426     C                   PARM                    SflRcd
192200160426     C                   PARM                    NotUsed
192300160426     C                   PARM                    SizeList
192400160426     C                   PARM                    NotUsed
192500160426     C                   PARM                    QUSEC
192600160426
192700160426     C                   ENDDO
192800160426
192900160426      * Next step - Signal end of input, clear subfile for reload.
193000160426     C                   EVAL      QLGRT00 = EndPut
193100160426     C                   CLEAR                   QUSEI
193200160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
193300160426
193400160426     C                   CALL      'QLGSRTIO'
193500160426     C                   PARM                    QLGSCB00
193600160426     C                   PARM                    SflRcd
193700160426     C                   PARM                    NotUsed
193800160426     C                   PARM                    SizeList
193900160426     C                   PARM                    NotUsed
194000160426     C                   PARM                    QUSEC
194100160426
194200160427     C                   SETOn                                        21
194300160427     C                   WRITE     LRF0c03
194400160427     C                   SETOff                                       21
194500160426
194600160426      * Final step - Write the records back to the subfile.
194700160426     C                   EVAL      QLGRT00 = Get
194800160426
194900160426     C                   DO        STMREC1       x
195000160426     C                   CLEAR                   QUSEI
195100160426     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
195200160426     C                   CALL      'QLGSRTIO'
195300160426     C                   PARM                    QLGSCB00
195400160426     C                   PARM                    NotUsed
195500160426     C                   PARM                    SflRcd
195600160426     C                   PARM                    QLGRL00
195700160426     C                   PARM                    NotUsed
195800160426     C                   PARM                    QUSEC
195900160426      *
196000160427     c                   z-add     x             nrr
196100160428     C     VSCDSN        IFEQ      'DS'
196200160428     C                   SETON                                        10
196300160428     C                   ELSE
196400160428     C                   SETOFF                                       10
196500160428     C                   ENDIF
196600160427     C                   WRITE     LRF0S03
196700160426     C                   ENDDO
196800160426      *
196900160426     C                   ENDSR
197000061002     C**********************************************************************
197100950525     C*--- CARICAMENTO SUBFILE ---------------------------------------*
197200061002     C**********************************************************************
197300950525     C     RIES03        BEGSR
197400170206     c                   setoff                                       44
197500950525     C                   Z-ADD     0             NRR               4 0
197600950525     C                   MOVEL(P)  V1CFPD        WPADR
197700950607     C* CARICO IN SCHIERA I PADRONCINI CHE HANNO GIA' 1 DISTINTA APERTA
197800050519     c
197900050519     c                   if        v05dfv = 0
198000950607     C                   Z-ADD     DATEU8        WDFV2
198100050519     c                   else
198200050519     C                   Z-ADD     V05DFV        G02DAT
198300050519     C                   MOVEL     *BLANK        G02ERR
198400050519     C                   CALL      'XSRDA8'
198500050519     C                   PARM                    WLBDAT
198600050519     C                   Z-ADD     G02INV        WDFV2
198700050519     c                   end
198800061002     C                   EXSR      LEGdst
198900061002     C* CARICO I PADRONCINI
199000021203     C     kapdw         SETLL     fiapd000
199100021203     C     apdtip        READe     fiapd000                               32
199200950525     C*
199300950525    1C     *IN32         DOWEQ     *OFF
199400000619     C*CONTROLLO SE INSERITO CODICE PARZIALIZZAZZIONE TIPO PADRONCINO
199500061011     C                   MOVE      *BLANK        PADROK            1
199600050519     c* se scelto messaggerie o parcel
199700050519     c                   movel     apdflr        dapdflr
199800050523     c                   if        v05tpm <> ' '
199900050523     c                   if        (§apdtpm = ' ' and v05tpm <> 'P') or
200000050523     c                             (§apdtpm = 'M' and v05tpm <> 'M')
200100050519     C                   MOVE      'X'           PADROK
200200050519     c                   end
200300050523     c                   end
200400170324      **
200500170324      ** chiamata pgm per verificare l'agenda e la presenza dell'autista
200600170324      ******    dateu8
200700170327     c                   clear                   FNLVF9DS
200800170327     c                   eval      ILVF9PDR = apdPDR
200900170327     c                   eval      ILVF9DATA=  WDFV2
201000170327     c                   call      'FNLVF9R'
201100170327     c                   parm                    FNLVF9DS
201200170327      ** da Escludere xché assente
201300170327     c                   if        OLVF9FLG = 'A'
201400170327     C                   MOVE      'X'           PADROK
201500170327     c                   end
201600170324      **
201700950525     C* ESCLUDO RECORD ANNULLATI
201800950525    2C     APDATB        IFEQ      ' '
201900140804     C* ESCLUDO I CODICI DI autisti occasionali
202000140804     C     APDsnu        ANDEQ     ' '
202100000619      * PADRONCINO CORRISPONDENTE ALLA PARZIALIZZAZIONE
202200170327      ** oppure perchè assente
202300000619     C     PADROK        ANDEQ     ' '
202400950525     C* CONTROLLO LA FILIALE DEL CODICE PADRONCINO
202500950525     C                   MOVEL     APDPDR        W0030             3 0
202600950525    3C     W0030         IFEQ      V1CFPD
202700950525     C                   CLEAR                   VSCSCE
202800950525     C                   MOVEL     APDPDR        VSCPDR
202900950525     C                   MOVEL     APDRSC        VSDPDR
203000070720     C                   MOVEL     §apdpda       VScpda
203100080207     C                   MOVEL     §apdtstpda    VSctstpda
203200080208     c* blink se aut. in test e procedura partita
203300080208     c                   if        wdfv2>= dateu8
203400080208     C                   if        (vsctstpda ='E' and
203500080208     C                             (§OGPDAORM ='S' and §OGPDACON='S'))
203600080208     C                             or (vsctstpda ='C' and
203700080208     C                             §OGPDACON='S')
203800080208     C                             or (vsctstpda ='O' and
203900080208     C                             §OGPDAORM='S')
204000080208     C                   seton                                        53
204100080208     c                   else
204200080208     c                   setoff                                       53
204300080208     C                   ENDIF
204400080208     c                   end
204500080208     c*
204600070726     c                   if        vscpda = ' '
204700070726     c                   eval      vscpda = 'N'
204800070726     c                   end
204900050519     c                   if        v05dfv = 0
205000050519     C                   MOVEL     UDATE8        VSCDFV
205100050519     C                   MOVEL     UDATE8        VHCDFV
205200050519     c                   else
205300050519     C                   MOVEL     v05dfv        VSCDFV
205400050519     C                   MOVEL     v05dfv        VHCDFV
205500050519     c                   end
205600950606     C* INDICO SE E' STATA GIA' APERTA IN QUEL GIORNO UNA DISTINTA PER
205700061002     C* QUEL PADRONCINO
205800950607     C     APDPDR        LOOKUP    PDR                                    10
205900950607     C   10              MOVEL     'DS'          VSCDSN
206000950607     C  N10              CLEAR                   VSCDSN
206100950525     C*
206200950525     C* PRESTAZIONE PADRONCINO (MATT/POM) E PIKING (S/N)
206300050719     c* se impostato nella finestra delle parzializzazioni
206400070111     c                   if        v05fpp <> *blanks
206500050719     C                   MOVEL     v05fpp        VSCfpp
206600070111     c                   end
206700070111    4C     HHMM          IFLE      1200
206800050719     c* lo imposto solo se non immesso nella finestra
206900070111     c                   if        vscfpp = *blanks
207000070111     C                   MOVEL     'M'           VSCFPP                         *MATTINO
207100070111     c                   end
207200070111   X4C                   ELSE
207300070111     c                   if        vscfpp = *blanks
207400070111     C                   MOVEL     'P'           VSCFPP                         *POMERIGGIO
207500070111     c                   end
207600070111    4C                   ENDIF
207700050923     c*
207800070111     c                   if        vscfpp = 'M'
207900050923     C     APDPKM        IFEQ      ' '
208000050923     C                   MOVEL     'N'           VSCPEP
208100050923     C                   ELSE
208200050923     C                   MOVEL     APDPKM        VSCPEP
208300050923     C                   ENDIF
208400050923     c                   else
208500050923     C     APDPKP        IFEQ      ' '
208600050923     C                   MOVEL     'N'           VSCPEP
208700050923     C                   ELSE
208800050923     C                   MOVEL     APDPKP        VSCPEP
208900050923     C                   ENDIF
209000050923     C                   ENDIF
209100050519     c*
209200150804     c                   if        *in50
209300150804     C                   MOVEL     §apdtpm       VSCtpm
209400150804     c                   else
209500150804     c                   clear                   vsctpm
209600150804     c                   end
209700150804     c                   if        vsctpm = ' '
209800050523     C                   MOVEL     'P'           VSCtpm
209900050523     c                   end
210000950525     C*
210100160426     c                   if        *in54
210200160426     C                   MOVEL     §apdturno     VSCtur
210300160426     c                   else
210400160426     c                   clear                   vsctur
210500160426     c                   end
210600160427     c                   if        vsctur = ' ' and §apdtpm = ' '
210700160426     C                   MOVEL     '1'           VSCtur
210800160426     c                   end
210900160426     C*
211000170207     c                   clear                   hPdrForza
211100170206     C*
211200950525     C                   ADD       1             NRR
211300950525     C                   WRITE     LRF0S03
211400950525     C*
211500950525   X3C                   ELSE
211600950525     C                   SETON                                        32
211700950525    3C                   ENDIF
211800950525    2C                   ENDIF
211900950525     C*
212000021203     C  N32apdtip        READe     fiapd000                               32
212100950525    1C                   ENDDO
212200160427     C*  Ordina per Codice Cliente
212300160428     c                   eval      Ordinamento   ='3'
212400160427     C                   Z-ADD     nrr           STMREC1
212500160426     c                   exsr      Ordina
212600950525     C*
212700950525     C                   Z-ADD     1             REC
212800950525     C                   ENDSR
212900061002     C**********************************************************************
213000950607     C*--- LETTURA DISTINTE APRTE IN UNA DATA ------------------------*
213100061002     C**********************************************************************
213200061002     C     LEGdst        BEGSR
213300950607     C                   CLEAR                   PDR
213400160427     C                   Z-ADD     1             Xy
213500070410     C     Kdst2         SETLL     fidst08l
213600061002     c                   do        *hival
213700070410     C     KDST2         reade     fidst08l
213800070410     c                   if        %eof(fidst08l)
213900061002     c                   leave
214000061002     c                   end
214100070209     C     dstATB        IFEQ      ' '
214200070209     C     dstFCF        ANDEQ     ' '
214300160427     C                   MOVEL     DSTPDR        PDR(Xy)
214400170207     C                   ADD       1             Xy                5 0
214500070209     c                   end
214600950607     C                   ENDDO
214700950607     C                   ENDSR
214800061002     C**********************************************************************
214900950525     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
215000061002     C**********************************************************************
215100950525     C     CTRS03        BEGSR
215200950526     C                   CLEAR                   WSCE
215300950607     C*
215400950607    0C     *IN09         IFEQ      *OFF
215500950607     C* SE NON HO PREMUTO F6-LEGGO IN READC PER CONTROLLARE SE HO
215600061002     C* MODIFICATO DELLE DATE
215700160426     C                   READC     LRF0S03                                31
215800160426    1C     *IN31         DOWEQ     *OFF
215900950607     C                   Z-ADD     NRR           REC
216000950607     C                   MOVEL     VSCPDR        V2CPDR
216100070720     C                   MOVEL     VScpda        V2cpda
216200080207     C                   MOVEL     VSctstpda     V2ctstpda
216300950607     C                   MOVEL     VSCDFV        V2CDFV
216400950607     C* CONTROLLI
216500950607     C                   EXSR      CTRDDC
216600971014     C  N28              Z-ADD     WDFV          VHRDFV
216700170206     c*----
216800170206      ***
216900170206     c*----
217000160426     c* turno
217100160429     C                   if        vsctpm = 'M' and vsctur <> ' '
217200160426     C                   MOVEL     MSG(29)       V2CMSG
217300160426     C                   SETON                                        5528
217400160426     C                   end
217500160429     C                   if        vsctpm = 'P' and vsctur = ' '
217600160429     C                   MOVEL     MSG(19)       V2CMSG
217700160429     C                   SETON                                        5528
217800160429     C                   end
217900160426     C                   UPDATE    LRF0S03
218000160426     C   28              SETON                                        31
218100160426     C  N31              READC     LRF0S03                                31
218200160426    1C                   ENDDO
218300160426     C   28              GOTO      ENDS03
218400160426    0C                   ENDIF
218500950526     C                   Z-ADD     1             NRR
218600950526     C     NRR           CHAIN     LRF0S03                            31
218700950526    1C     *IN31         DOWEQ     *OFF
218800950526     C* WSCE = "1" --> E' STATO SELEZIONATO ALMENO UN RECORD DEL SFL
218900950526     C     VSCSCE        IFEQ      '1'
219000950526     C                   MOVEL     '1'           WSCE              1
219100950526     C                   SETON                                        31
219200950526     C                   ENDIF
219300950526     C*
219400950526     C                   ADD       1             NRR
219500950526     C  N31NRR           CHAIN     LRF0S03                            31
219600950526    1C                   ENDDO
219700950607     C* LEGGO IL SFL PER CONTROLLARE LA VALIDITA' DEI DATI IMMESSI
219800061002     c*
219900950607     C* SOLO SE E' STATO PREMUTO CMD6-CONFERMA GLOBALE PERCHE'
220000950607     C*   ALTRIMENTI I CONTROLLI LI EFFETTUO PASSANDO DA OGNI VIDEATA
220100950607    0C     *IN09         IFEQ      *ON                                          *CMD6
220200950525     C                   Z-ADD     1             NRR
220300950525     C     NRR           CHAIN     LRF0S03                            31
220400950525    1C     *IN31         DOWEQ     *OFF
220500950529     C                   SETOFF                                       4546
220600041008     C                   SETOFF                                       474849
220700160426     C                   SETOFF                                       515255
220800170206     C                   SETOFF                                       44
220900950526     C* WSCE = "1" --> CONTROLLO SOLO I RECORD DEL SFL SELEZIONATI
221000950526    2C     WSCE          IFEQ      '1'
221100950526     C     VSCSCE        ANDEQ     '1'
221200950526     C* WSCE = " " --> CONTROLLO TUTTI I RECORD DEL SFL
221300950526     C     WSCE          OREQ      ' '
221400950526     C* IMPOSTO CAMPI UTILIZZATI NELLE ROUTINE COMUNI
221500070720     C                   MOVEL     VScpda        V2cpda
221600080207     C                   MOVEL     VSctstpda     V2ctstpda
221700070123     C                   MOVEL     VSCPDR        V2CPDR
221800950525     C                   MOVEL     VSCDFV        V2CDFV
221900950526     C                   MOVEL     VSCFPP        V2CFPP
222000950526     C                   MOVEL     VSCPEP        V2CPEP
222100050519     C                   MOVEL     VSCtpm        V2Ctpm
222200160426     C                   MOVEL     VSCtur        V2Ctur
222300070108     C                   MOVEL     'S'           V2Cfca
222400950526     C* CONTROLLI
222500950525     C                   EXSR      CTRD02
222600170206      **----
222700170207      **-  ATTENZIONE AL DISACCREDITATO FORZATO O NO
222800170207      **  se NON ha Forzato  si memorizza "9999999" nel codice Hidden
222900170207      *    e prosegue senza fermarsi accendendo solo una segnalazione di codice
223000170208     c                   if        *in44
223100170207     c                   setoff                                       28
223200170207     c                   end
223300170207      **----
223400950525     C   28              Z-ADD     NRR           REC
223500971014     C  N28              Z-ADD     WDFV          VHRDFV
223600950525     C*
223700950525     C                   UPDATE    LRF0S03
223800950525     C   28              SETON                                        31
223900950526    2C                   ENDIF
224000950525     C                   ADD       1             NRR
224100950525     C  N31NRR           CHAIN     LRF0S03                            31
224200950525    1C                   ENDDO
224300950607    0C                   ENDIF
224400950607     C*
224500950607     C     ENDS03        ENDSR
224600061002     C**********************************************************************
224700950525     C*--- AGGIORNAMENTO SUBFILE -------------------------------------*
224800061002     C**********************************************************************
224900950525     C     AGGS03        BEGSR
225000061002     c*
225100061003     C                   Z-ADD     1             NRR
225200061003     C     NRR           CHAIN     LRF0S03                            31
225300061003    1C     *IN31         DOWEQ     *OFF
225400950526     C* WSCE = "1" --> AGGIORNO SOLO I RECORD DEL SFL SELEZIONATI
225500950526    2C     WSCE          IFEQ      '1'
225600950526     C     VSCSCE        ANDEQ     '1'
225700950526     C* WSCE = " " --> AGGIORNO TUTTI I RECORD DEL SFL
225800950526     C     WSCE          OREQ      ' '
225900170207      *
226000170207     C*  Se deve essere saltato
226100170207    2C                   IF        hPDRforza <> *ALL'9'
226200170207      *
226300950526     C* IMPOSTO CAMPI UTILIZZATI NELLE ROUTINE COMUNI
226400070720     C                   MOVEL     VScpda        V2cpda
226500080207     C                   MOVEL     VSctstpda     V2ctstpda
226600070123     C                   MOVEL     VSCPDR        V2CPDR
226700950525     C                   MOVEL     VSDPDR        V2DPDR
226800950525     C                   MOVEL     VSCDFV        V2CDFV
226900950525     C                   MOVEL     VSCFPP        V2CFPP
227000950525     C                   MOVEL     VSCPEP        V2CPEP
227100050519     C                   MOVEL     VSCtpm        V2Ctpm
227200160426     C                   MOVEL     VSCtur        V2Ctur
227300070516     C                   MOVEL     'S'           V2Cfca
227400950607     C                   CLEAR                   V2CNOT
227500950607     C     VSCDSN        IFEQ      'DS'
227600950607     C                   SETON                                        10
227700950607     C                   ELSE
227800950607     C                   SETOFF                                       10
227900950607     C                   ENDIF
228000950526     C** CMD1  - CONFERMA PER DISTINTA: PASSA DALLA SINGOLA VIDEATA
228100950526    3C     *IN08         IFEQ      *ON
228200950525     C                   EXSR      GESD02
228300950525     C* RECORD FILE NUMERATORI ALLOCATO: RITORNO IN PRIMA VIDEATA
228400061002     C   39              leave
228500950526    3C                   ENDIF
228600950525     C** CMD6  - AGGIORNA SENZA PASSARE DALLA SINGOLA VIDEATA
228700950526    3C     *IN09         IFEQ      *ON
228800950529     C                   MOVEL     V1CFGS        V2CFGS
228900950703     C* AGGIORNAMENTO
229000971014     C                   Z-ADD     VHRDFV        WDFV
229100950525     C                   EXSR      AGGD02
229200170207     c*
229300061229     c* aggiorno archivi FNFVV e FNDST
229400061229     c                   if        not *in28
229500061229     c                   eval      d05cmd ='KF'
229600061229     c                   exsr      sraggold
229700061229     c                   end
229800070322     c*
229900070322     c                   if        *in28
230000070322     c                   rolbk
230100070322     C                   leave
230200070322     c                   else
230300061229     c                   commit
230400070322     c                   end
230500950529     C                   MOVEL     V2CNFV        V1CNFP                         *DIST.PRECEDENTE
230600950529     C* SE RICHIESTA ESEGUO STAMPA ETICHETTA PADRONCINO
230700950529    4C     V1CSTE        IFEQ      'S'
230800950529     C                   CLEAR                   DSLV25
230900950529     C                   MOVEL     V2CPDR        D25PDR
231000950529     C                   MOVEL     V2DPDR        D25RSC
231100070328     C                   eval      d25nfv = V2CnFV
231200950529     C                   MOVEL     V2CDFV        D25DDC
231300080310     c                   eval      D25fpp = V2Cfpp
231400950529     C                   MOVEL     V2CNOT        D25NOT
231500950908     C                   CALL      PA1PSD
231600950529     C                   PARM                    DSLV25
231700950529    4C                   ENDIF
231800950529    3C                   ENDIF
231900950526    2C                   ENDIF
232000170207     c                   end
232100170207      *---
232200070108     c                   seton                                        10
232300070108     C   10              MOVEL     'DS'          VSCDSN
232400070108     c                   update    lrf0s03
232500061003     C                   ADD       1             NRR
232600061003     C  N31NRR           CHAIN     LRF0S03                            31
232700950525    1C                   ENDDO
232800950525     C*
232900061002     C                   ENDSR
233000050519     C**********************************************************************
233100050519     C*--- GESTIONE VIDEATA DELLE SELEZIONI --------------------------*
233200050519     C**********************************************************************
233300050519     C     GESD05        BEGSR
233400050617     C                   SETOFF                                       45
233500050519     C                   CLEAR                   V05tpm
233600050519     C                   eval      V05dfv = udate8
233700050719    2C     HHMM          IFLE      1200
233800050719     C                   MOVEL     'M'           V05FPP                         *MATTINO
233900050719     c                   else
234000050719     C                   MOVEL     'P'           V05FPP                         *MATTINO
234100050719     c                   end
234200050519     C* EMISSIONE FORMATO
234300050519     C                   do        *hival
234400050519     C                   EXFMT     LRF0D05
234500050617     C                   SETOFF                                       4528
234600050519     C* CONTROLLo data distinta
234700050519     C                   MOVEL     v05dfv        V2CDFV
234800160427     c                   clear                   vhcdfv
234900160427     c                   clear                   vhrdfv
235000050519     C                   EXSR      CTRDdc
235100050617     C                   MOVEL     v2cdfv        V05DFV
235200050519     C   28              iter
235300050519     c                   leave
235400050519     c                   enddo
235500050519     C*
235600050519     C                   ENDSR
235700061002     C**********************************************************************
235800061222     C*  chiamo fidg02r x gestire gli altri dati della distinta
235900061002     C**********************************************************************
236000061002     C     sraltro       BEGSR
236100061002     C*
236200070720     c                   clear                   okagg             1
236300070720     c                   clear                   fidg02ds
236400070122     C* SE SONO IN IMMISSIONE passo comando KJ
236500070122    1C     *IN01         IFEQ      *ON                                          *IMMISSIONE
236600070122     C     *IN03         OREQ      *ON                                          *CMD15
236700070122     c                   eval      d02cmd = 'KJ'
236800070913     c* questo flag mi serve per simulare l'inserimento anche se sono in
236900070913     c* modifica xchè ho già inserito il record
237000070913     c                   eval      d02ana = '1'
237100070122     c                   else
237200070122     c                   eval      d02cmd = 'KF'
237300070122     c                   end
237400061222     c                   eval      d02fil = v2cfgs
237500070122     c                   eval      d02tipa= 'A'
237600061222     c                   eval      d02pdr = v2cpdr
237700061222     c                   eval      d02rsc = v2dpdr
237800070320     c                   eval      d02strcmt = 'N'
237900070320     c                   eval      d02opncmt = 'S'
238000070320     c                   eval      d02com = 'N'
238100061222     c                   eval      d02file= 'FIDST00F'
238200061222     c                   move      wnpg          d02npg
238300061222     c                   move      v2cdfv        dtaeur
238400061006     c                   move      dtaeur        dtaiso
238500061222     c                   move      dtaiso        d02dfv
238600061222     c                   eval      d02nfv = v2cnfv
238700061222     c                   eval      d02fpp = v2cfpp
238800061222     c                   eval      d02tpm = v2ctpm
238900070720     c                   eval      d02tp  = 'C'
239000061222     c                   eval      kpjbu = fidg02ds
239100070723     c* faccio 2 giri: uno per consegne uno per ritiri solo se abilitato
239200070726     c                   eval      yy =1
239300070726     c                   if        §OGDDAO <> ' '
239400070726     c                   eval      yy = 2
239500070726     c                   end
239600070726     c                   do        yy            xx                1 0
239700070720     c                   if        xx = 2
239800070720     c                   eval      d02tp  = 'R'
239900070720     c* se inseriti i giri x consegne, x i ritiri entro in aggiornamento
240000070720     c* altrimenti lascio inserimento
240100070913     c                   if        okagg = '1'
240200070913     c                   eval      d02cmd = 'KF'
240300070913     c                   end
240400070720     c                   eval      kpjbu = fidg02ds
240500070720     c                   end
240600061222     c                   call      'FIDG02R'
240700061002     c                   parm                    kpjba
240800061222     c                   movel(p)  kpjbu         fidg02ds
240900070720     c                   if        xx = 1 and
241000070720     c                             d02cmde = 'KF'
241100070720     c                   eval      okagg = '1'
241200070720     c                   end
241300070720     c                   enddo
241400070720     c*
241500070720     c                   if        okagg = '1' and d02cmde <> 'KF'
241600070720     c                   eval      d02cmde = 'KF'
241700070720     c                   end
241800061002     C*
241900061002     C                   ENDSR
242000070717     C**************************************************************************
242100070717     C* controllo e decodifico giro
242200070717     C**************************************************************************
242300070717     C     Srcgi         BEGSR
242400070717     C*
242500070717     c                   eval      D09IOP0 = '001'
242600070717     c                   move      dstdfv        d09idat
242700070717     c                   eval      kpjbu = fidg09ds
242800070717     c                   call      'FIDG09R'
242900070717     c                   parm                    kpjba
243000070717     c                   eval      fidg09ds = kpjbu
243100070717     C*
243200070717     C                   ENDSR
243300061002     C**********************************************************************
243400061002     C*--- INZSR INIZIALE --------------------------------------------*
243500061002     C**********************************************************************
243600061002     C     *INZSR        BEGSR
243700061002     C*
243800061002     C     *ENTRY        PLIST
243900061002     C                   PARM                    KPJBA
244000061002     C                   MOVEL     KPJBU         PARAM1
244100061002      * Reperisco dati job
244200061002     c                   exsr      DatiJob
244300061002
244400061002     c     Karb78        Klist
244500061002     c                   Kfld                    Karbifp
244600061002     c                   Kfld                    Karbndc
244700070223     c     Karg          Klist
244800070223     c                   Kfld                    Kargfgs
244900070223     c                   Kfld                    Kargndc
245000061002     C***
245100061002     c     Kapdw         Klist
245200061002     c                   Kfld                    apdtip
245300061002     c                   Kfld                    wpadr
245400061002     c     Kapdd         Klist
245500061002     c                   Kfld                    apdtip
245600061002     c                   Kfld                    dstpdr
245700061002     c                   movel     'A'           apdtip
245800061222     c     Kapd21        Klist
245900061222     c                   Kfld                    apdtip
246000061227     c                   Kfld                    v2cpdr
246100061227     c                   Kfld                    v2cfpp
246200061222     c                   Kfld                    comiso
246300061222     c     Kapd2         Klist
246400061222     c                   Kfld                    apdtip
246500061227     c                   Kfld                    v2cpdr
246600061227     c                   Kfld                    v2cfpp
246700110223     c     Kpct          Klist
246800110223     c                   Kfld                    dstFGS
246900110223     c                   Kfld                    dstNFV
247000110223     c                   Kfld                    dstpdr
247100110324     c                   Kfld                    wtrd              3
247200110324     c                   eval       wtrd  = 'CET'
247300061002     C***
247400061002     C     KTBE          KLIST
247500061002     C                   KFLD                    KCOD1
247600061002     C                   KFLD                    KKE1
247700061002     C*  tabelle dgd x reperimento limiti chiusura spedizioni
247800061002     C                   MOVEL     'DGD'         KCOD1
247900061002     C                   MOVEL     *BLANKS       KKE1
248000061002     C                   MOVEL     '1'           KKE1
248100061002     C     KTBE          CHAIN     TNTBE01L                           31
248200061002     C     *IN31         IFEQ      '0'
248300061002     C                   MOVEL     TBEUNI        DDGD
248400061002     C                   END
248500061002     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
248600061002     C***
248700061002     C                   TIME                    W0140            14 0
248800061002     C                   MOVE      W0140         UDATE8            8 0
248900061002     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
249000061002     C*
249100061002     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
249200061002     C                   MOVEL     *BLANK        G02ERR
249300061002     C                   CALL      'XSRDA8'
249400061002     C                   PARM                    WLBDAT
249500061002     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
249600070424     c* se ricevuto p.o. imposto e proteggo
249700070424     c                   if        pa1fgs <> ' '
249800070424     C                   MOVEL     pa1fgs        V1CFGS                         *FIL.GESTIONE
249900070424     C                   MOVEL     pa1fgs        V1CFPD                         *FIL.PADRONCINO
250000070424     C                   SETON                                        05
250100070424     c                   else
250200061003     C                   MOVEL     dutpou        V1CFGS                         *FIL.GESTIONE
250300061003     C                   MOVEL     dutpou        V1CFPD                         *FIL.PADRONCINO
250400061003     c                   if        dutlpo = '2'
250500061002     C                   SETON                                        05
250600061002    1C                   ENDIF
250700070424    1C                   ENDIF
250800061002     C*
250900061002     C* WGES = '1' ===> GESTIONE VIDEATA DELLE SELEZIONI
251000061002     C                   MOVEL     '1'           WGES              1
251100061002     C*
251200061227     C                   MOVEL     'FIDG01R'     V1CPGM
251300061003     C                   Z-ADD     4             WNPG              1 0
251400061002     C                   Z-ADD     12            WCNU
251500061002     C                   MOVEL     'C'           WFVL
251600061002     C                   CLEAR                   WTSR
251700061002     C                   CLEAR                   V1CNFP
251800061002     C                   MOVEL     'S'           V1CSTE
251900080411     C*****              MOVEL     'N'           V1CSNB
252000061002     C*
252100061002     C***
252200061002     C* DEFINIZIONE CAMPI
252300061002     C***
252400061002     C     *LIKE         DEFINE    UDATE8        DATEU8
252500061002     C     *LIKE         DEFINE    DSTPDR        WPADR
252600061002     C     *LIKE         DEFINE    dstFGS        WFIL
252700061002     C     *LIKE         DEFINE    dstNFV        WNFV
252800061002     C     *LIKE         DEFINE    dstDFV        WDFV
252900061002     C     *LIKE         DEFINE    dstDFV        WDFV2
253000061002     C     *LIKE         DEFINE    i33CNU        WCNU
253100061002     C     *LIKE         DEFINE    FTTFVL        WFVL
253200061002     C     *LIKE         DEFINE    FTTTSR        WTSR
253300061227     C     *LIKE         DEFINE    TBEKE1        KKE1
253400061227     C     *LIKE         DEFINE    TBECOD        KCOD1
253500061002     C*
253600061228     C* ACCESSO fidst
253700061011     C     Kdst1         KLIST
253800061010     C                   KFLD                    V1CFGS
253900061010     C                   KFLD                    WNFV
254000061011     C     Kdst2         KLIST
254100070410     C                   KFLD                    V1CFGS
254200070209     C                   KFLD                    WNPG
254300061011     C                   KFLD                    Wdfv2
254400080319     C     Kdst4         KLIST
254500080319     C                   KFLD                    wnpg
254600080319     C                   KFLD                    wnfv
254700080319     C                   KFLD                    v1cfgs
254800080319     C                   KFLD                    dsftrd
254900061002     C*
255000061228     C     Kdst          KLIST
255100070410     C                   KFLD                    V1CFGS
255200061002     C                   KFLD                    WNPG
255300061002     C                   KFLD                    Wnfv
255400070410     C     Kbrv          KLIST
255500070410     C                   KFLD                    WNPG
255600070410     C                   KFLD                    Wnfv
255700070410     C                   KFLD                    V1CFGS
255800061002     C* ACCESSO Fiqdt01l
255900061002     C     Kqdt          KLIST
256000061002     C                   KFLD                    V1CFGS
256100061002     C                   KFLD                    WNFV
256101170919     C                   KFLD                    wdfvex
256200061002     C* ACCESSO FIFTT02L + FIFST02L
256300061002     C     KFTT          KLIST
256400061002     C                   KFLD                    V2CPDR
256500061002     C                   KFLD                    WFVL
256600061002     C                   KFLD                    WDFV
256700061002     C                   KFLD                    WTSR
256800061002     C* ACCESSO FNORM06L
256900061002     C     KORM          KLIST
257000061002     C                   KFLD                    V1CFGS
257100061002     C                   KFLD                    wnfv
257200061002     C                   ENDSR
257300061002      *---------------------------------------------------------------*
257400061002      * Reperimento Dati del job (Utente/Operativi)                   *
257500061002      *---------------------------------------------------------------*
257600061002     c     DatiJob       BEGSR
257700061002      *
257800061002     c     *dtaara       define    §azute        azuteds
257900061002     c     *dtaara       define    §datiute      ddatiute
258000061002      *
258100061002     c                   in(E)     *dtaara
258200061002     c                   IF        %ERROR or RSUT = *blanks
258300061002     c                   clear                   Tibs34Ds
258400061002     c                   call      'TIBS34R'
258500061002     c                   parm                    Tibs34Ds
258600061002     c                   in        *dtaara
258700061002     c                   ENDIF
258800061002      *
258900061002     c                   ENDSR
259000950525     C*---------------------------------------------------------------*
259100950529     OFNLV22P   T    LRNLR
259200950523**  MSG
259300970416Foglio Distinta inesistente o in gestione ad altro punto operativo            01
259400950524Foglio Distinta al momento non disponibile: riprovare piu' tardi              02
259500950524Foglio Distinta gia' chiuso                                                   03
259600950628Il Foglio Distinta immesso non e' al momento manutenzionabile
259700950524Foglio Distinta non aperto con questo programma: impossibile la manutenzione  05
259800970416Autotrasportat. diverso da quello gia' presente in Distinta: ENTER per forzare
259900071214Impossibile modificare l'autotrasp.: presenti bolle/O.R.M. in distinta        07
260000970416Immettere il Codice Autotrasportatore                                         08
260100970416Con l'inserimento multiplo non e' possibile digitare il Codice Autotrasport.  09
260200970416Codice Autotrasportatore inesistente                                          10
260300970416P.O. Autotrasportatore incongruente con punto operativo apertura foglio
260400950524Tasto di funzione non ammesso se e' stato digitato il Foglio Distinta         12
260500950703Ora non e' possibile creare nuovi Fogli Distinta: attendere e ripremere F6    13
260600950525Impossibile annullare il Foglio Distinta: presenti bolle o spunte !!          14
260700950525Data immessa errata                                                           15
260800980430Data inferiore alla data corrente o superiore al limite imposto
260900950525Data errata: gia' confermati i conteggi !!                                    17
261000950525Immettere il Numero Bolle                                                     18
261100950526Il valore immesso per il campo non e' valido                                  19
261200950628La distinta e' ora in uso e non aggiornabile: attendere e ripremere F6        20
261300010314Impossibile annullare il Foglio Distinta: presenti O.R.M.!!                   21
261400020910Manca il numeratore dei Fogli Distinte: TELEFONARE IN SEDE!!!!                22
261500041008Incongruenza fra il numero dei bancali affidati e il flag di gestione         23
261600070516Non esiste un'anagrafica autotrasportatore completa. Occorre inserirla        24
261700061229Distinta non aggiornata. CONTROLLARE!!!                                       25
261800101022Filiale non ancora attiva per la gestione PDA scelta                          26
261900110224Scelta incongruente o non ammessa                                             27
262000080319La distinta è in una fase dove non è + possibile modificare il valore         28
262100160428Il turno è ammesso solo se tipo distinta è Parcel                             29
262200170227Autista BLOCCATO !!                                                           30
