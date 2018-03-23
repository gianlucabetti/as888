000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200151201      * tibs89r *----------------------------------------------------*
000300090116      *    - Duplica dati di fisgn per etichettare in altro TERMINAL
000400151201      *        più filiali
000500940915      *--------------------------------------------------------------*
000600151201     FTIBS89D   CF   E             WORKSTN
000700951013     FAZORG01L  IF   E           K DISK
000800090116     Ffisgn07l  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200130108     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001300130108     d vksc            s              7    dim(5)
001400151201     d fil             s              3  0 dim(12)
001500151201     d tfp             s              3  0 dim(12)
001600130109     d                 ds
001700130109     D v1dks1                  1     20
001800130109     D v1dks2                 21     40
001900130109     D v1dks3                 41     60
002000130109     D v1dks4                 61     80
002100130109     D v1dks5                 81    100
002200130109     D v1dksc                  1    100
002300130109     d dksc                          20    dim(5)  overlay(v1dksc)
002400040917     D KPJBA         E DS
002500040917     D CNCR80        E DS
002600040917     D UT§DSE0F      E DS
002700130108     d  tcu                  398    697    DIM(50)
002800130108     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
002900090116     D fnlv55ds      E DS
003000090116     D podatfp         S              3  0
003100090116     D poatfp          S              3  0
003200151201     D v1cpoa          S              3  0
003300151201     D v1cpoda         S              3  0
003400130108     d                 ds
003500130108     D parksc                        35
003600130109     D ksc                            7    dim(5) overlay(parksc)
003700130108
003800130109     d TIBS69DS      E DS
003900130109     d DS_cnaco      E DS                  extname(CNACO00F)
004000130109     d DS_cnind      E DS                  extname(CNIND00F)
004100130109     d DS_cnclp      E DS                  extname(CNCLP00F)
004200130109     d DS_fncls      E DS                  extname(FNCLS00F)
004300130108     d DSXc          E DS                  extname(xclirds)
004400130108     d TCUDS           DS
004500130108     d  f1                     1      1
004600130108     d  f3                     3      3
004700130108     d  f2                     2      2
004800130108     d  f4                     4      4
004900130108     d  f56                    5      6
005000130108     d                 ds
005100130108     ddsksn                    1      4p 0
005200130108     ddsksa                    1      4
005300130109     D wtibs69r        s              1
005400130109     D XX              s              3  0
005500151201     Ibs89d01
005600130108     I              v1cks1                      vksc(1)
005700130108     I              v1cks2                      vksc(2)
005800130108     I              v1cks3                      vksc(3)
005900130108     I              v1cks4                      vksc(4)
006000130108     I              v1cks5                      vksc(5)
006100050103     C**********************************************************************
006200040917     C     *ENTRY        PLIST
006300040917     C                   PARM                    KPJBA
006400050103     c*
006500040917     C                   Z-ADD     1             CODUT
006600040917     C                   CALL      'X§PARUT'
006700040917     C                   PARM                    UT§DSE0F
006800040917     C                   MOVEL     REC80         CNCR80
006900040917     C                   MOVEL     RAGUT         VIDRSU
007000130108      *--- RICERCA CAPOCONTI
007100130108     C                   DO        50            XX
007200130108     C                   MOVE      TCU(XX)       TCUDS
007300130108     C     f56           CABNE     'CG'          END1
007400130108     C     f4            COMP      '1'                                    21
007500130108     C     f4            COMP      '2'                                    22
007600130108     C     f4            COMP      '3'                                    23
007700130108     C     f4            COMP      '6'                                    27
007800130108     C** 1 CLIENTI   21
007900130108     C** 2 FORNITORI 22
008000130108     C** 3 AGENTI    23
008100130108     C     f3            comp      '0'                                242425
008200130108     C     f3            comp      'I'                                    26
008300130108     C** 0 ITALIA   25
008400130108     C** 1 ESTERO   24
008500130108     ** I CAPO CONTO IVA
008600130108     c                   if        *in21 = *on
008700130108     C   24              Z-ADD     KCU(XX)       KCE               4 0
008800130108     C   25              Z-ADD     KCU(XX)       KCI               4 0
008900130108     c                   end
009000130108     c                   if        *in22 = *on
009100130108     C   24              Z-ADD     KCU(XX)       KFE               4 0
009200130108     C   25              Z-ADD     KCU(XX)       KFI               4 0
009300130108     c                   end
009400130108     c                   if        *in23 = *on
009500130108     C   24              Z-ADD     KCU(XX)       KAE               4 0
009600130108     C   25              Z-ADD     KCU(XX)       KAI               4 0
009700130108     c                   end
009800130108     C   26              Z-ADD     KCU(XX)       KIVA              4 0
009900130108     C   27              Z-ADD     KCU(XX)       KBNA              4 0
010000130108     C     end1          tag
010100130108     C                   end
010200130108     C                   SETOFf                                       212223
010300130108     C                   sETOFf                                       242526
010400130108     C                   SETOFf                                       27
010500130108
010600151201     C                   CLEAR                   V1CPOda1
010700151201     C                   CLEAR                   V1CPOda2
010800151201     C                   CLEAR                   V1CPOda3
010900151201     C                   CLEAR                   V1CPOda4
011000151201     C                   CLEAR                   V1CPOda5
011100151201     C                   CLEAR                   V1CPOda6
011200151201     C                   CLEAR                   V1CPOda7
011300151201     C                   CLEAR                   V1CPOda8
011400151201     C                   CLEAR                   V1CPOda9
011500151201     C                   CLEAR                   V1CPOda10
011600151201     C                   CLEAR                   V1CPOda11
011700151201     C                   CLEAR                   V1CPOda12
011800130108     C                   CLEAR                   vksc
011900130108     c                   clear                   dksc
012000040917     c
012100040917     C     FOR01         TAG
012200000828      *
012300151201     C                   EXFMT     BS89D01
012400970102     C                   SETOFF                                       289050
012500040917     C* F3 - FINE
012600040917     C   KC              GOTO      FINE
012700041220      *  CONTROLLI
012800040917     C                   EXSR      CTRD01
012900040917     c   90              goto      for01
013000000825      *
013100040917     c*
013200090116     c                   if        not *inkf
013300041220     c                   goto      for01
013400041220     c                   endif
013500040917     C*
013600040917      * F6  - SOTTOMETTO ELABORAZIONE
013700050412    1C     *INKF         IFEQ      *ON
013800050214     c*
013900130109     c                   movea     vksc          ksc
014000151201     c* Sottometto tutte le combinazioni di linee
014100151201     c                   z-add     1             xx
014200151201     c                   do        12            xx
014300151201     c                   if        fil(xx)>0
014400151201     c                   z-add     1             zz                3 0
014500151201     c                   do        12            zz
014600151201     c                   if        zz<>xx and fil(zz)>0
014700151201     c                   eval      v1cpoda=fil(xx)
014800151201     c                   eval      v1cpoa =tfp(zz)
014900151201     c                   eval      podatfp=tfp(xx)
015000151201     c                   exsr      esegui
015100151201     c                   endif
015200151201     c                   enddo
015300151201     c
015400151201     c                   endif
015500151201     c                   enddo
015600151201     c
015700090116    1C                   ENDIF
015800050412     c
015900900515      *
016000000000     C     FINE          TAG
016100090116     C*
016200090116     c                   clear                   fnlv55ds
016300090116     C                   MOVEL     'C'           D55TLA
016400090116     C                   CALL      'FNLV55R'
016500090116     C                   PARM                    FNLV55DS
016600000000     C                   SETON                                        LR
016700151201      *-----------------------------------------------------***********
016800151201     c     esegui        BEGSR
016900151201     c                   if        parksc<>*blanks
017000151201     c                   call      'TIBS92R'
017100151201     c                   parm                    v1cpoda
017200151201     c                   parm                    v1cpoa
017300151201     c                   parm                    podatfp
017400151201     c                   parm                    parksc
017500151201     c                   else
017600151201     c                   call      'TIBS92R'
017700151201     c                   parm                    v1cpoda
017800151201     c                   parm                    v1cpoa
017900151201     c                   parm                    podatfp
018000151201     c                   endif
018100151201     c                   ENDSR
018200151201      *-----------------------------------------------------***********
018300151201     c     CTRtrasfe     BEGSR
018400151201      *
018500151201     c                   if        v1cpoda>0
018600151201     C                   MOVE      V1CPOda       CODFIL            3 0
018700151201     C                   EXSR      CTRFIL
018800151201    1c                   if        not *in34
018900151201     c
019000151201   x1C                   ELSE
019100151201     c                   seton                                          9028
019200151201     C                   MOVEL     MSG(1)        V1cMSG
019300151201     c                   leavesr
019400151201    1C                   ENDIF
019500151201     c
019600151201     c* controllo il suo terminal partenza
019700151201     c                   clear                   fnlv55ds
019800151201     C                   MOVEL     'P'           D55TPT
019900151201     c                   eval      d55lin=v1cpoda
020000151201     c                   eval      d55drf=*date
020100151201     C                   CALL      'FNLV55R'
020200151201     C                   PARM                    FNLV55DS
020300151201     c                   eval      podatfp=d55tfp
020400151201     c
020500151201     c                   if        %lookup(v1cpoda:fil)>0
020600151201     c                   seton                                          9028
020700151201     C                   MOVEL     MSG(2)        V1cMSG
020800151201     c                   leavesr
020900151201    1C                   ENDIF
021000151201     c                   if        %lookup(podatfp:tfp)>0
021100151201     c                   seton                                          9028
021200151201     C                   MOVEL     MSG(2)        V1cMSG
021300151201     c                   leavesr
021400151201    1C                   ENDIF
021500151201     c                   eval      fil(xx)=v1cpoda
021600151201     c                   eval      tfp(xx)=podatfp
021700151201    1C                   ENDIF
021800151201     c                   ENDSR
021900900515      *-----------------------------------------------------***********
022000090116      * CONTROLLI                                           *  CTRD01 *
022100900515      *-----------------------------------------------------***********
022200040917     C     CTRD01        BEGSR
022300050104     c                   setoff                                       010206
022400130109     C                   CLEAR                   H1RIGA
022500130109     C                   CLEAR                   H1COLO
022600151201     c                   clear                   tfp
022700151201     c                   clear                   fil
022800151201
022900151201     c* controllo le 12 filiali
023000151201     c                   z-add     1             xx                3 0
023100151201     c                   eval      v1cpoda=v1cpoda1
023200151201     c                   exsr      ctrtrasfe
023300151201     c                   if        *in90
023400151201     c                   seton                                        50
023500151201     c                   goto      endct1
023600151201     c                   endif
023700151201     c* controllo le 12 filiali
023800151201     c                   z-add     2             xx                3 0
023900151201     c                   eval      v1cpoda=v1cpoda2
024000151201     c                   exsr      ctrtrasfe
024100151201     c                   if        *in90
024200151201     c                   seton                                        51
024300151201     c                   goto      endct1
024400151201     c                   endif
024500151201
024600151201     c                   z-add     3             xx                3 0
024700151201     c                   eval      v1cpoda=v1cpoda3
024800151201     c                   exsr      ctrtrasfe
024900151201     c                   if        *in90
025000151201     c                   seton                                        52
025100151201     c                   goto      endct1
025200151201     c                   endif
025300151201
025400151201     c                   z-add     4             xx                3 0
025500151201     c                   eval      v1cpoda=v1cpoda4
025600151201     c                   exsr      ctrtrasfe
025700151201     c                   if        *in90
025800151201     c                   seton                                        53
025900151201     c                   goto      endct1
026000151201     c                   endif
026100151201
026200151201     c                   z-add     5             xx                3 0
026300151201     c                   eval      v1cpoda=v1cpoda5
026400151201     c                   exsr      ctrtrasfe
026500151201     c                   if        *in90
026600151201     c                   seton                                        54
026700151201     c                   goto      endct1
026800151201     c                   endif
026900151201
027000151201     c                   z-add     6             xx                3 0
027100151201     c                   eval      v1cpoda=v1cpoda6
027200151201     c                   exsr      ctrtrasfe
027300151201     c                   if        *in90
027400151201     c                   seton                                        55
027500151201     c                   goto      endct1
027600151201     c                   endif
027700151201
027800151201     c                   z-add     7             xx                3 0
027900151201     c                   eval      v1cpoda=v1cpoda7
028000151201     c                   exsr      ctrtrasfe
028100151201     c                   if        *in90
028200151201     c                   seton                                        56
028300151201     c                   goto      endct1
028400151201     c                   endif
028500151201
028600151201     c                   z-add     8             xx                3 0
028700151201     c                   eval      v1cpoda=v1cpoda8
028800151201     c                   exsr      ctrtrasfe
028900151201     c                   if        *in90
029000151201     c                   seton                                        57
029100151201     c                   goto      endct1
029200151201     c                   endif
029300151201
029400151201     c                   z-add     9             xx                3 0
029500151201     c                   eval      v1cpoda=v1cpoda9
029600151201     c                   exsr      ctrtrasfe
029700151201     c                   if        *in90
029800151201     c                   seton                                        58
029900151201     c                   goto      endct1
030000151201     c                   endif
030100151201
030200151201     c                   z-add     10            xx                3 0
030300151201     c                   eval      v1cpoda=v1cpoda10
030400151201     c                   exsr      ctrtrasfe
030500151201     c                   if        *in90
030600151201     c                   seton                                        59
030700151201     c                   goto      endct1
030800151201     c                   endif
030900151201
031000151201     c                   z-add     11            xx                3 0
031100151201     c                   eval      v1cpoda=v1cpoda11
031200151201     c                   exsr      ctrtrasfe
031300151201     c                   if        *in90
031400151201     c                   seton                                        60
031500151201     c                   goto      endct1
031600151201     c                   endif
031700151201
031800151201     c                   z-add     12            xx                3 0
031900151201     c                   eval      v1cpoda=v1cpoda12
032000151201     c                   exsr      ctrtrasfe
032100151201     c                   if        *in90
032200151201     c                   seton                                        61
032300151201     c                   goto      endct1
032400151201     c                   endif
032500090116     c*
032600130108     c*
032700130108     c* Parzializzazione per codice cliente
032800130108     c* controllo codici clienti
032900130108     c                   do        5             ii                2 0
033000130108     c     vksc(ii)      ifne      *blanks
033100130108     c     vksc(ii)      andne     *zeros
033200130108     C     '?'           SCAN      vksc(ii)                               32
033300130108IF  2C     *in32         ifeq      *on
033400130108     C                   clear                   dsxc
033500130108     C                   z-add     kci           DXCCAP
033600130108     C                   CALL      'XCLIR'
033700130108     C                   PARM                    DSXC
033800130108     C*
033900130108     c                   MOVEl     DXCKSC        dsksa                      ALF
034000130108     C                   move      dsksn         w0070             7 0      M
034100130108     C                   move      w0070         vksc(ii)                   M
034200130108     c                   seton                                        90
034300130108     c                   goto      endct1
034400130108     c                   endif
034500130108     C* CODICE CLIENTE NUMERICO
034600130108     C                   SETOFf                                       32
034700130108     C                   TESTN                   Vksc(ii)             32
034800130108IF  3C     *in32         ifeq      *on
034900130108     C                   move      vksc(ii)      W001              1
035000130108IF  4C     w001          iflt      '0'
035100130108     c                   seton                                        2890
035200130108     c                   exsr      rie_riga
035300130108     c                   z-add     45            h1colo
035400130108     C                   movel     msg(5)        v1cmsg
035500130108     C                   goto      endct1
035600130108E   4C                   ENDIF
035700130108X   3C                   ELSE
035800130108     c                   seton                                        2890
035900130108     c                   exsr      rie_riga
036000130108     c                   z-add     45            h1colo
036100130108     C                   movel     msg(5)        v1cmsg
036200130108     C                   goto      endct1
036300130108E   3C                   ENDIF
036400130108     c*
036500130108     C                   clear                   TIBS69DS
036600130108     c                   move      vksc(ii)      I69kac
036700130108      *
036800130108     C                   CALL      'TIBS69R'
036900130108     C                   PARM                    tibs69DS
037000130108     C                   PARM                    DS_cnaco
037100130108     C                   PARM                    DS_cnind
037200130108     C                   PARM                    DS_cnclp
037300130108     C                   PARM                    DS_fncls
037400130108      *
037500130108IF  2C                   eval      *IN28 = (O69ERR =  '1')
037600130108     C                   eval      wtibs69r = '1'
037700130108     c                   if        *in28
037800130108     c                   seton                                        90
037900130108     c                   exsr      rie_riga
038000130108     c                   z-add     45            h1colo
038100130108     c                   movel     msg(5)        v1cmsg
038200130108     c                   goto      endct1
038300130108     c                   endif
038400130108     c                   movel     acorag        dksc(ii)
038500130108     c* errore se codice cliente ripetuto
038600130108     c                   z-add     1             yy                2 0
038700130108     c     vksc(ii)      lookup    vksc(yy)                               30
038800130108     c   30ii            ifne      yy
038900130108     c                   seton                                        2890
039000130108     c                   if        *in28
039100130108     c                   exsr      rie_riga
039200130108     c                   z-add     45            h1colo
039300130108     c                   movel     msg(6)        v1cmsg
039400130108     c                   goto      endct1
039500130108     c                   endif
039600130108     c                   endif
039700130108     c                   else
039800130108     c                   clear                   dksc(ii)
039900130108     c                   end
040000130108     c                   enddo
040100041220     c*
040200900515      *
040300040917     C     ENDCT1        ENDSR
040400130108     C**************************************************************************
040500130108     C*  Impostazione numero di riga per posizionamento cursore
040600130108     C**************************************************************************
040700130108     C     rie_riga      BEGSR
040800130108     c                   select
040900130108     c                   when      ii=1
041000130108     c                   z-add     16            h1riga
041100130108     c                   when      ii=2
041200130108     c                   z-add     17            h1riga
041300130108     c                   when      ii=3
041400130108     c                   z-add     18            h1riga
041500130108     c                   when      ii=4
041600130108     c                   z-add     19            h1riga
041700130108     c                   when      ii=5
041800130108     c                   z-add     20            h1riga
041900130108     c                   endsl
042000130108     c                   endsr
042100951025     C**************************************************************************
042200951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
042300951025     C**************************************************************************
042400951025     C     CTRFIL        BEGSR
042500020213
042600951025     C     CODFIL        CHAIN     AZORG01L                           34
042700951025    1C  N34ORGFVA        IFNE      ' '
042800951025     C     ORGFAG        ORNE      'A'
042900951025     C     ORGFAG        ANDNE     'F'
043000951025     C                   SETON                                        34
043100951025    1C                   ENDIF
043200951025     C*
043300951025     C                   ENDSR
043400020805      **************************************************************************
043500960302** SCHIERA MSG - MESSAGGI DI ERRORE
043600090116Filiale  INESISTENTE                                                          01
043700151201Le filiali  devono essere diverse e avere terminal di partenza diversi        01
043800090116Non è presente nessun record in FISGN00F per la filiale immessa
043900090116si può trasferire solo ad un terminal di partenza
044000130108Codice cliente errato                                                          5
044100130108Codice cliente ripetuto                                                        6
