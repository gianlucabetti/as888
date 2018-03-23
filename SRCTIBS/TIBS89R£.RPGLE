000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200090116      * tibs91r *----------------------------------------------------*
000300090116      *    - Duplica dati di fisgn per etichettare in altro TERMINAL
000400940915      *--------------------------------------------------------------*
000500090116     FTIBS91D   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700090116     Ffisgn07l  IF   E           K DISK
000800020805      *
000900020805      * DEFINIZIONE SCHIERE
001000960302     D*
001100130108     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001101130108     d vksc            s              7    dim(5)
001102130109     d                 ds
001104130109     D v1dks1                  1     20
001105130109     D v1dks2                 21     40
001106130109     D v1dks3                 41     60
001107130109     D v1dks4                 61     80
001108130109     D v1dks5                 81    100
001109130109     D v1dksc                  1    100
001110130109     d dksc                          20    dim(5)  overlay(v1dksc)
001200040917     D KPJBA         E DS
001300040917     D CNCR80        E DS
001400040917     D UT§DSE0F      E DS
001401130108     d  tcu                  398    697    DIM(50)
001402130108     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
001500090116     D fnlv55ds      E DS
001600090116     D podatfp         S              3  0
001700090116     D poatfp          S              3  0
001701130108     d                 ds
001702130108     D parksc                        35
001703130109     D ksc                            7    dim(5) overlay(parksc)
001704130108
001705130109     d TIBS69DS      E DS
001706130109     d DS_cnaco      E DS                  extname(CNACO00F)
001707130109     d DS_cnind      E DS                  extname(CNIND00F)
001708130109     d DS_cnclp      E DS                  extname(CNCLP00F)
001709130109     d DS_fncls      E DS                  extname(FNCLS00F)
001710130108     d DSXc          E DS                  extname(xclirds)
001711130108     d TCUDS           DS
001712130108     d  f1                     1      1
001713130108     d  f3                     3      3
001714130108     d  f2                     2      2
001715130108     d  f4                     4      4
001716130108     d  f56                    5      6
001717130108     d                 ds
001718130108     ddsksn                    1      4p 0
001719130108     ddsksa                    1      4
001720130109     D wtibs69r        s              1
001721130109     D XX              s              3  0
001722130108     Ibs91d01
001723130108     I              v1cks1                      vksc(1)
001724130108     I              v1cks2                      vksc(2)
001725130108     I              v1cks3                      vksc(3)
001726130108     I              v1cks4                      vksc(4)
001727130108     I              v1cks5                      vksc(5)
001800050103     C**********************************************************************
001900040917     C     *ENTRY        PLIST
002000040917     C                   PARM                    KPJBA
002100050103     c*
002200040917     C                   Z-ADD     1             CODUT
002300040917     C                   CALL      'X§PARUT'
002400040917     C                   PARM                    UT§DSE0F
002500040917     C                   MOVEL     REC80         CNCR80
002600040917     C                   MOVEL     RAGUT         VIDRSU
002601130108      *--- RICERCA CAPOCONTI
002602130108     C                   DO        50            XX
002603130108     C                   MOVE      TCU(XX)       TCUDS
002604130108     C     f56           CABNE     'CG'          END1
002605130108     C     f4            COMP      '1'                                    21
002606130108     C     f4            COMP      '2'                                    22
002607130108     C     f4            COMP      '3'                                    23
002608130108     C     f4            COMP      '6'                                    27
002609130108     C** 1 CLIENTI   21
002610130108     C** 2 FORNITORI 22
002611130108     C** 3 AGENTI    23
002612130108     C     f3            comp      '0'                                242425
002613130108     C     f3            comp      'I'                                    26
002614130108     C** 0 ITALIA   25
002615130108     C** 1 ESTERO   24
002616130108     ** I CAPO CONTO IVA
002617130108     c                   if        *in21 = *on
002618130108     C   24              Z-ADD     KCU(XX)       KCE               4 0
002619130108     C   25              Z-ADD     KCU(XX)       KCI               4 0
002620130108     c                   end
002621130108     c                   if        *in22 = *on
002622130108     C   24              Z-ADD     KCU(XX)       KFE               4 0
002623130108     C   25              Z-ADD     KCU(XX)       KFI               4 0
002624130108     c                   end
002625130108     c                   if        *in23 = *on
002626130108     C   24              Z-ADD     KCU(XX)       KAE               4 0
002627130108     C   25              Z-ADD     KCU(XX)       KAI               4 0
002628130108     c                   end
002629130108     C   26              Z-ADD     KCU(XX)       KIVA              4 0
002630130108     C   27              Z-ADD     KCU(XX)       KBNA              4 0
002631130108     C     end1          tag
002632130108     C                   end
002633130108     C                   SETOFf                                       212223
002634130108     C                   sETOFf                                       242526
002635130108     C                   SETOFf                                       27
002636130108
002700090116     C                   CLEAR                   V1CPOda
002800090116     C                   CLEAR                   V1DPOda
002900090116     C                   CLEAR                   V1Cpoa
003000090116     C                   CLEAR                   V1dpoa
003001130108     C                   CLEAR                   vksc
003002130108     c                   clear                   dksc
003100040917     c
003200040917     C     FOR01         TAG
003300000828      *
003400090116     C                   EXFMT     BS91D01
003500970102     C                   SETOFF                                       289050
003600040917     C* F3 - FINE
003700040917     C   KC              GOTO      FINE
003800041220      *  CONTROLLI
003900040917     C                   EXSR      CTRD01
004000040917     c   90              goto      for01
004100000825      *
004200040917     c*
004300090116     c                   if        not *inkf
004400041220     c                   goto      for01
004500041220     c                   endif
004600040917     C*
004700040917      * F6  - SOTTOMETTO ELABORAZIONE
004800050412    1C     *INKF         IFEQ      *ON
004900050214     c*
004901130109     c                   movea     vksc          ksc
004902130109     c                   if        parksc<>*blanks
005000090116     c                   call      'TIBS92R'
005100090116     c                   parm                    v1cpoda
005200090116     c                   parm                    v1cpoa
005300090119     c                   parm                    podatfp
005301130108     c                   parm                    parksc
005302130109     c                   else
005303130109     c                   call      'TIBS92R'
005304130109     c                   parm                    v1cpoda
005305130109     c                   parm                    v1cpoa
005306130109     c                   parm                    podatfp
005307130109     c                   endif
005400090116    1C                   ENDIF
005500050412     c
005600900515      *
005700000000     C     FINE          TAG
005800090116     C*
005900090116     c                   clear                   fnlv55ds
006000090116     C                   MOVEL     'C'           D55TLA
006100090116     C                   CALL      'FNLV55R'
006200090116     C                   PARM                    FNLV55DS
006300000000     C                   SETON                                        LR
006400900515      *-----------------------------------------------------***********
006500090116      * CONTROLLI                                           *  CTRD01 *
006600900515      *-----------------------------------------------------***********
006700040917     C     CTRD01        BEGSR
006800050104     c                   setoff                                       010206
006801130109     C                   CLEAR                   H1RIGA
006802130109     C                   CLEAR                   H1COLO
006900090116      * PUNTO OPERATIVO  DA
007000090116     C                   MOVE      V1CPOda       CODFIL            3 0
007100951025     C                   EXSR      CTRFIL
007200041220    1c                   if        not *in34
007300090116     c                   movel     orgdes        V1DPOda
007400040917     c
007500041220   x1C                   ELSE
007600040917     c                   seton                                        509028
007700041220     C                   MOVEL     MSG(1)        V1cMSG
007800041220     c                   goto      endct1
007900041220    1C                   ENDIF
008000090116     c* Verifico se presenti dei dati
008100090116     c     v1cpoda       setll     fisgn07l
008200090116     c                   if        not %equal(fisgn07l)
008300090116     c                   seton                                        509028
008400090116     C                   MOVEL     MSG(3)        V1cMSG
008500090116     c                   goto      endct1
008600090116     c                   endif
008700090116     c
008800090116     c* controllo il suo terminal partenza
008900090116     c                   clear                   fnlv55ds
009000090116     C                   MOVEL     'P'           D55TPT
009100090116     c                   eval      d55lin=v1cpoda
009200090116     c                   eval      d55drf=*date
009300090116     C                   CALL      'FNLV55R'
009400090116     C                   PARM                    FNLV55DS
009500090116     c                   eval      podatfp=d55tfp
009600090116     c*
009700090116      * PUNTO OPERATIVO  A  deve essere un terminal
009800090116     C                   MOVE      V1CPOa        CODFIL            3 0
009900090116     C                   EXSR      CTRFIL
010000090116    1c                   if        not *in34
010100090116     c                   movel     orgdes        V1DPOa
010200090116     c
010300090116   x1C                   ELSE
010400090116     c                   seton                                        519028
010500090116     C                   MOVEL     MSG(1)        V1cMSG
010600090116     c                   goto      endct1
010700090116    1C                   ENDIF
010800090116     c* controllo il suo terminal partenza
010900090116     c                   clear                   fnlv55ds
011000090116     C                   MOVEL     'P'           D55TPT
011100090116     c                   eval      d55lin=v1cpoa
011200090116     c                   eval      d55drf=*date
011300090116     C                   CALL      'FNLV55R'
011400090116     C                   PARM                    FNLV55DS
011500090116     c                   eval      poatfp=d55tfp
011600090116
011700090116     c                   if        poatfp<>v1cpoa
011800090116     c                   seton                                        519028
011900090116     C                   MOVEL     MSG(4)        V1cMSG
012000090116     c                   goto      endct1
012100090116    1C                   ENDIF
012200090116     c
012300090119     c* Devono essere diversi sia le filiali che i terminal
012400090116     c                   if        v1cpoda=v1cpoa  or  podatfp=poatfp
012500090116     c                   seton                                        519028
012600090116     C                   MOVEL     MSG(2)        V1cMSG
012700090116     c                   goto      endct1
012800090116    1C                   ENDIF
012801130108     c*
012802130108     c* Parzializzazione per codice cliente
012803130108     c* controllo codici clienti
012804130108     c                   do        5             ii                2 0
012805130108     c     vksc(ii)      ifne      *blanks
012806130108     c     vksc(ii)      andne     *zeros
012807130108     C     '?'           SCAN      vksc(ii)                               32
012808130108IF  2C     *in32         ifeq      *on
012809130108     C                   clear                   dsxc
012810130108     C                   z-add     kci           DXCCAP
012811130108     C                   CALL      'XCLIR'
012812130108     C                   PARM                    DSXC
012813130108     C*
012814130108     c                   MOVEl     DXCKSC        dsksa                      ALF
012815130108     C                   move      dsksn         w0070             7 0      M
012816130108     C                   move      w0070         vksc(ii)                   M
012817130108     c                   seton                                        90
012818130108     c                   goto      endct1
012819130108     c                   endif
012820130108     C* CODICE CLIENTE NUMERICO
012821130108     C                   SETOFf                                       32
012822130108     C                   TESTN                   Vksc(ii)             32
012823130108IF  3C     *in32         ifeq      *on
012824130108     C                   move      vksc(ii)      W001              1
012825130108IF  4C     w001          iflt      '0'
012826130108     c                   seton                                        2890
012827130108     c                   exsr      rie_riga
012828130108     c                   z-add     45            h1colo
012829130108     C                   movel     msg(5)        v1cmsg
012830130108     C                   goto      endct1
012831130108E   4C                   ENDIF
012832130108X   3C                   ELSE
012833130108     c                   seton                                        2890
012835130108     c                   exsr      rie_riga
012836130108     c                   z-add     45            h1colo
012837130108     C                   movel     msg(5)        v1cmsg
012838130108     C                   goto      endct1
012839130108E   3C                   ENDIF
012840130108     c*
012841130108     C                   clear                   TIBS69DS
012842130108     c                   move      vksc(ii)      I69kac
012843130108      *
012844130108     C                   CALL      'TIBS69R'
012845130108     C                   PARM                    tibs69DS
012846130108     C                   PARM                    DS_cnaco
012847130108     C                   PARM                    DS_cnind
012848130108     C                   PARM                    DS_cnclp
012849130108     C                   PARM                    DS_fncls
012850130108      *
012851130108IF  2C                   eval      *IN28 = (O69ERR =  '1')
012855130108     C                   eval      wtibs69r = '1'
012856130108     c                   if        *in28
012857130108     c                   seton                                        90
012858130108     c                   exsr      rie_riga
012859130108     c                   z-add     45            h1colo
012860130108     c                   movel     msg(5)        v1cmsg
012861130108     c                   goto      endct1
012862130108     c                   endif
012863130108     c                   movel     acorag        dksc(ii)
012864130108     c* errore se codice cliente ripetuto
012865130108     c                   z-add     1             yy                2 0
012866130108     c     vksc(ii)      lookup    vksc(yy)                               30
012867130108     c   30ii            ifne      yy
012868130108     c                   seton                                        2890
012869130108     c                   if        *in28
012870130108     c                   exsr      rie_riga
012871130108     c                   z-add     45            h1colo
012872130108     c                   movel     msg(6)        v1cmsg
012873130108     c                   goto      endct1
012874130108     c                   endif
012875130108     c                   endif
012876130108     c                   else
012877130108     c                   clear                   dksc(ii)
012878130108     c                   end
012879130108     c                   enddo
012900041220     c*
013000900515      *
013100040917     C     ENDCT1        ENDSR
013101130108     C**************************************************************************
013102130108     C*  Impostazione numero di riga per posizionamento cursore
013103130108     C**************************************************************************
013104130108     C     rie_riga      BEGSR
013105130108     c                   select
013106130108     c                   when      ii=1
013107130108     c                   z-add     16            h1riga
013108130108     c                   when      ii=2
013109130108     c                   z-add     17            h1riga
013110130108     c                   when      ii=3
013111130108     c                   z-add     18            h1riga
013112130108     c                   when      ii=4
013113130108     c                   z-add     19            h1riga
013114130108     c                   when      ii=5
013115130108     c                   z-add     20            h1riga
013116130108     c                   endsl
013117130108     c                   endsr
013200951025     C**************************************************************************
013300951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
013400951025     C**************************************************************************
013500951025     C     CTRFIL        BEGSR
013600020213
013700951025     C     CODFIL        CHAIN     AZORG01L                           34
013800951025    1C  N34ORGFVA        IFNE      ' '
013900951025     C     ORGFAG        ORNE      'A'
014000951025     C     ORGFAG        ANDNE     'F'
014100951025     C                   SETON                                        34
014200951025    1C                   ENDIF
014300951025     C*
014400951025     C                   ENDSR
014500020805      **************************************************************************
014600960302** SCHIERA MSG - MESSAGGI DI ERRORE
014700090116Filiale  INESISTENTE                                                          01
014800090116Le 2 filiali  devono essere diverse e avere terminal di partenza diversi      01
014900090116Non è presente nessun record in FISGN00F per la filiale immessa
015000090116si può trasferire solo ad un terminal di partenza
015100130108Codice cliente errato                                                          5
015200130108Codice cliente ripetuto                                                        6
