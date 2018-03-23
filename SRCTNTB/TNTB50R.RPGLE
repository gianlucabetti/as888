000100020207     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020207      *--------------------------------------------------------------------------------------------*
000300051202      *        GESTIONE TABELLA 3TR PER abilitazioni trasferimento dati EDI                        *
000400020207      *--------------------------------------------------------------------------------------------*
000500020207      * Data base                                                                                  *
000600020207      *--------------------------------------------------------------------------------------------*
000700051202     FTnTb50d   CF   E             WORKSTN
000800051202     F                                     SFILE(tb50s1:nrr)
000900020207     Ftntbe01l  IF   E           K DISK
001000020207     Ftntbe02l  IF   E           K DISK    rename(tntbe000:tntbe002)
001100020207     FAzOrg01l  IF   E           K DISK
001200020207      *--------------------------------------------------------------------------------------------*
001300020207      * Data structure                                                                             *
001400020207      *--------------------------------------------------------------------------------------------*
001500051202     D msg             S             78    DIM(5) CTDATA PERRCD(1)              *Messaggi video
001600051202     D skipo           S              3    DIM(10)
001700020207      *---
001800020207     D Nrr             S              4  0                                      *n° rel sf
001900020207     D $Fine           S              1    INZ('N')                             *fine programma
002000020207     D TipVid          S              1    INZ('1')                             *video in gestione
002100020207     D $CarSfl         S              1    INZ('0')                             *video in gestione
002200020207     D $CarVd1         S              1    INZ('1')                             *video in gestione
002300020207     D WMax            S                   like($nrr)
002400020207      *---
002500020207      * Ds
002600020207      *---
002700051202     D Tntb50Ds      E DS                                                       *ds di procedura
002800051206     D d3tr          E DS                                                       *ds di procedura
002900020207     D Tibs02Ds      E DS                                                       *ds di procedura
003000020207     D Tibs69Ds      E DS                                                       *ds di procedura
003100020207     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
003200020207     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
003300020207     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
003400020207     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
003500020207      *
003600020207     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700020207     D  TCU                  398    697
003800020207     D                                     DIM(50)
003900020207     D  KCU                  698    847P 0
004000020207     D                                     DIM(50)
004100020207      *
004200020207     D TCUDS           DS
004300020207     D  F1                     1      1
004400020207     D  F3                     3      3
004500020207     D  F2                     2      2
004600020207     D  F4                     4      4
004700020207     D  F56                    5      6
004800020207      *---
004900020207     D kpjba         E DS                                                       *architettura
005000020207      *--------------------------------------------------------------------------------------------*
005100020207      * Main lines
005200020207      *--------------------------------------------------------------------------------------------*
005300020207      *
005400020207      * Operazioni iniziali
005500020207      *
005600020207     C                   EXSR      RutInz
005700020207      *
005800020207      * Gestione video
005900020207      *
006000020207     C     $Fine         DOUEQ     'S'
006100020207     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
006200020207     C     TipVid        CASEQ     '2'           GesSfl                          *Sfile
006300010921     C                   ENDCS
006400010921     C                   ENDDO
006500020207      *
006600010921     C                   SETON                                        LR
006700020207      *--------------------------------------------------------------------------------------------*
006800020207      * GesVd1 - Gestione videata selezione codice tabella -
006900020207      *--------------------------------------------------------------------------------------------*
007000020207     C     GesVd1        BEGSR
007100020207      *
007200020207      * Inizializzazione videata
007300020207     C     $CarVd1       ifeq      *on
007400020207     C                   EXSR      CarVd1
007500020207     C                   move      *off          $CarVd1
007600020207     C                   endif
007700020207      *
007800051202     C                   EXFMT     tb50v1
007900020207      *
008000020207     C                   select
008100020207      * F3=Fine
008200020207     C     *inKc         wheneq    *on
008300020207     C                   EXSR      F03Ges
008400051202     c                   other
008500020207      *
008600020207      *
008700020207      * Controlli e decodifiche
008800020208     C                   EXSR      CtrV1
008900020207      *
009000020207     C     *IN28         ifeq      *off
009100020211     C     Ric_Est       andeq     *off
009200020207      *
009300020207      * Gestisco il video di dettaglio
009400020207     C                   move      *on           $CarSfl
009500020207     C                   move      '2'           TipVid
009600020207     C                   endif
009700020207      *
009800020207     C                   endsl
009900020207      *
010000020207      *
010100020207     C                   endsr
010200020207      *--------------------------------------------------------------------------------------------*
010300020207      * CarVd1 - Carica primo video
010400020207      *--------------------------------------------------------------------------------------------*
010500020207     C     CarVd1        BEGSR
010600020207      *
010700020207     C                   setoff                                       284041
010800020211      *
010900051202     C                   clear                   tb50v1
011000020207     C                   movel     Ragut         V1CRag           20
011100020207     C                   movel     Knmus         V1CUte           10
011200020207     C                   movel     KnSif         V1CSif           10
011300051202     C                   movel     '       '     V1CCod
011400020207      *
011500020207     C                   ENDSR
011600020207      *--------------------------------------------------------------------------------------------*
011700020207      * Controllo e decodifica prima videata
011800020207      *--------------------------------------------------------------------------------------------*
011900020208     C     CtrV1         BEGSR
012000020207      *
012100020208     C                   setoff                                       284041
012200020211     C                   move      *off          Ric_Est           1
012300020207     C                   clear                   V1CMsg
012400051202     c                   if        v1cpou<>0 and v1ccod<>*blanks
012500051202     c                   movel     msg(5)        v1cmsg
012600051202     c                   seton                                        28
012700051202     C                   goto      FineSr
012800051202     c                   endif
012900020207      *
013000020208 2a  C     v1cpou        ifne      *zeros
013100020208     C     v1cpou        chain     Azorg01l                           21
013200020208     C   21              seton                                        2841
013300051202     c   21              movel     msg(4)        v1cmsg
013400020208     C   21              goto      FineSr
013500020208 2-  C                   endif
013600020208      *
013700020208 1a  C     v1ccod        ifeq      *blanks
013800051202     c     v1dcod        andne     *blanks
013900020207      *
014000020207      * Ricerca estesa
014100020207     C
014200020207     C                   movel     ragut         paxdut           30
014300020207     C                   movel(P)  v1dcod        paxdmt           48
014400020207      *
014500020207     C                   z-add     0             paxsta
014600020211     C                   movel     $Kci          paxccc            4 0
014700020207     C                   clear                   paxdit
014800020207     C                   z-add     1             paxnum
014900020207      *
015000020207     C                   CALL      'XALFA3BR'
015100020207     C                   PARM                    PAXDUT
015200020207     C                   PARM                    CODUT
015300020207     C                   PARM                    PAXDMT
015400020207     C                   PARM                    PAXCCC
015500020207     C                   PARM                    PAXSTA            1 0
015600020207     C                   PARM                    PAXFLR           90
015700020207     C                   PARM                    PAXDIT            3
015800020207     C                   PARM                    PAXNUM            2 0
015900020207     C                   PARM                    PAXKCM           80
016000020207     C                   PARM                    PAXKSM          140
016100020207     C                   PARM                    PAXKDM           60
016200020207      *
0163000202082a   C     PaxSta        ifgt      -1
016400020207     C                   movel     paxksm        v1ccod
016500020207     C                   movel     paxdmt        v1dcod
0166000202082-   C                   endif
016700051202      * segnalo che provengo da ricerca estesa per
016800020211      * riemettere video.
016900020211     C                   move      *on           Ric_Est           1
017000020208     C                   goto      Finesr
0171000202081-   C                   endif
017200020207      *
017300051202      * codice cliente
017400020208     C                   movel     *blanks       v1dcod
0175000512021a   C     v1ccod        ifne      *blanks
017600020208      * Controllo se il codice è numerico
017700020208     C                   testn                   v1ccod               31
017800020208     C                   move      v1ccod        w001a             1
0179000202082a   C     *in31         ifeq      *on
018000020208     C     w001a         andge     '0'
018100020208     C                   move      v1ccod        $Cod              7 0
018200020208     C                   EXSR      DeCod
018300020208      *
0184000202083a   C                   if        o69err = *blanks and acoflg =' '
018500020207     C                   movel     acorag        v1dcod
0186000202083e   C                   else
018700020208     C                   seton                                        2840
0188000202083-   C                   endif
018900020208      *
0190000202082e   C                   else
019100020208     C                   seton                                        2840
0192000202082-   C                   endif
0193000202081-   C                   endif
019400020207      *
019500020208     C     FineSr        ENDSR
019600020207      *--------------------------------------------------------------------------------------------*
019700020207      * Gessfl - gestione video subfile
019800020207      *--------------------------------------------------------------------------------------------*
019900020207     C     GesSfl        BEGSR
020000010924      *
020100010924      * azzero indicatori
020200020207     C     $CarSfl       ifeq      *on
020300020207     C                   exsr      CarSfl
020400010921     C                   endif
020500010927      *
020600010927      * scrivo il piede
020700051202     C                   write     tb50z1
020800011017      * se non ci sono dati da caricare e non ho richesto il posizionamento
020900010927     C     wmax          ifeq      0
021000020207     C                   seton                                        71
021100020207     C                   setoff                                       70
021200051202     C                   write     tb50v2                                       NO RCD
021300011017     C                   endif
021400010927      * emetto il control
021500051202     C                   exfmt     tb50c1
021600020207      *
021700020207     C                   select
021800020207      *
021900020207      * F03=Fine
022000020207     C     *inkc         wheneq    *on
022100020207     C                   EXSR      F03Ges
022200020207      *
022300020207      * F10=Immissione
022400020207     C     *inkj         wheneq    *on
022500020211     C                   EXSR      F10Ges
022600020207      *
022700020207      * F12=Ritorno
022800020207     C     *inkl         wheneq    *on
022900020207     C                   EXSR      F12Ges
023000020207     C                   EndSl
023100010928      *
023200020207      * Ho qualcosa di caricato da controllare
023300020207     C     wmax          ifgt      *zeros
023400020207     C                   EXSR      CtrS1
023500010928     C                   endif
023600020207      *
023700010921     C                   ENDSR
023800020207      *--------------------------------------------------------------------------------------------*
023900020207      * CarSfl - caricamento sfile
024000020207      *--------------------------------------------------------------------------------------------*
024100020207     C     CarSfl        BEGSR
024200020207      *
024300010928     C                   setoff                                           90
024400020207     C                   z-add     *zeros        Nrr                            *n° record sfile
024500020207      * pulizia sfile
024600020207     C                   seton                                        7071      *non visualizza sfl
024700051202     C                   write     tb50c1
024800020207     C                   setoff                                       7071
024900020207      *
025000020207      * caricamento sfile -tutto-
025100020207     C                   EXSR      ScrSfl                                       *scrive sfile
025200020207      *
025300010921     C                   ENDSR
025400020207      *--------------------------------------------------------------------------------------------*
025500020207      * ScrSfl - scrive il sfile -tutto-
025600020207      *--------------------------------------------------------------------------------------------*
025700020207     C     ScrSfl        BEGSR
025800020207      *
025900020207      * imposta dati
026000051202     C                   movel     Ragut         v1cRag
026100051202     C                   movel     Knmus         v1cUte
026200051202     C                   movel     KnSif         v1cSif
026300010927     C                   z-add     0             wmax
026400051202     c                   clear                   tbeke1
026500051202     c                   clear                   tbeke2
026600020207      *
026700051202     C                   move      '3TR'         tbecod
026800020208     C                   move      *blanks       tbelin
026900020207     C                   movel     $sif          tbesif
027000051202     c                   if        v1cpou>0
027100051202     C                   movel(P)  v1cpou        tbeke1
027200051202     c                   endif
027300051202     c                   if        v1ccod<>*blanks
027400051202     C                   movel(P)  v1ccod        tbeke2
027500051202     c                   movel     v1ccod        w0030             3 0
027600051202     C                   movel(P)  w0030         tbeke1
027700051202     c                   endif
027800010927      *
027900010927      * mi posiziono anche per codice
028000051202     c                   if        v1ccod<>*blaNKS
028100051202     C     ktbe5         setll     tntbe02l
028200051202     C                   ELSE
028300051202     C     ktbe4         setll     tntbe02l
028400051202     C                   ENDIF
028500051202     C
028600051202     C     ktbe3         reade     tntbe02l                               97
028700010924      *
028800010924      * Ciclo di lettura sulla tabella da caricare
028900020207     C                   dow       not *in97
029000010924      *
029100010924      * riempimento campi subfile
029200020207     C                   exsr      RieS1
029300011017      * imposto attributi su sfl
029400011017     C                   Exsr      AtrS1
029500010924      * scrive sfile
029600010924     C                   add       1             nrr                            *incrementa n°rec
029700010927      *
029800010927      * mi posiziono sul primo record caricato
029900010927     C                   z-add     1             $nrr                           *posiziona in fondo
030000051202     C                   write     tb50s1                                       *scrive sfile
030100020207     C                   z-add     nrr           wmax
030200010921      *
030300051202     C     ktbe3         reade     tntbe02l                               97
030400020207     C                   ENDDO
030500020207     C                   move      '0'           $CarSfl
030600020207      *
030700010921     C                   ENDSR
030800020207      *--------------------------------------------------------------------------------------------*
030900020207      * F03ges - Tasto funzionale f03 -> Fine programma
031000020207      *--------------------------------------------------------------------------------------------*
031100020207     C     F03ges        BEGSR
031200020207      *
031300020207     C                   movel     'S'           $Fine                          *fine pgm
031400020207      *
031500010921     C                   ENDSR
031600020211      *--------------------------------------------------------------------------------------------*
031700020211      * F10ges - Tasto funzionale f10 -> Immissione
031800020211      *--------------------------------------------------------------------------------------------*
031900020211     C     F10ges        BEGSR
032000020211      *
032100051202     C                   reset                   Tntb50ds
032200051202     C                   move      *blanks       b50ke1
032300051202     C                   move      *blanks       b50ke2
032400051202     C                   eval      b50Fun='F10'
032500051202     C                   EXSR      callb51r
032600020211      *
032700020211     C                   move      *on           $CarSfl
032800020211     C                   z-add     1             $nrr                           *posiziona in fondo
032900020211      *
033000020211     C                   ENDSR
033100020207      *--------------------------------------------------------------------------------------------*
033200020207      * F12ges - Tasto funzionale f12 -> Ritorno
033300020207      *--------------------------------------------------------------------------------------------*
033400020207     C     F12ges        BEGSR
033500020207      *
033600020207     C                   move      '1'           TipVid
033700020212     C                   move      *on           $CarVd1
033800020207      *
033900020207     C                   ENDSR
034000020207      *--------------------------------------------------------------------------------------------*
034100051202      * callb51r Richiamo tntb51r nelle diverse modalità
034200020207      *--------------------------------------------------------------------------------------------*
034300051202     C     callb51r      BEGSR
034400010924      *
034500020207      * valorizzo Ds di richiamo
034600010924     C
034700051202     C                   eval      b50lin=' '
034800051202     C                   eval      b50sif=tbesif
034900051202     C                   movel     $Kci          B50KCI
035000051202     C                   movel     $apl          B50apl
035100010924      *
035200010924      * Richiamo pgm di gestione
035300051202     C                   call      'TNTB51R'
035400010925     C                   PARM                    kpjba
035500051202     C                   PARM                    tntb50ds
035600010924      *
035700051202     C     b50fun        ifeq      'F03'
035800051202     C     b50err        oreq      *on
035900020207     C                   eval      $fine = 'S'
036000020207     C                   endif
036100020207      *
036200010924     C                   ENDSR
036300020207      *--------------------------------------------------------------------------------------------*
036400020207      * CtrS1  - Controlla campi sfile
036500020207      *--------------------------------------------------------------------------------------------*
036600020207     C     CtrS1         BEGSR
036700020207      *
036800011008     C                   setoff                                         9082
036900020208     C                   setoff                                         2842
037000010924      *
037100010924      * lettura dei record "toccati" del sfile
037200010924     C                   z-add     1             nrr
037300051202     C                   readc     tb50s1                                 82
037400010927     C                   dow       not *in82
037500010924      *
037600010924      * gestione opzioni
037700010924      *
037800010924      * Opzioni
037900020207     C                   if        s1copz <> ' '
038000010924      *
038100020207     C                   if        s1copz = '2'  and s1cAnn <>'A'or
038200020207     C                             s1copz = '4'  and s1cAnn <>'A'or
038300020207     C                             s1copz = '5'  or
038400020207     C                             s1copz = '7'  and s1cAnn ='A'
038500010926      *
038600051202     C                   reset                   tntb50ds
038700051202     C                   eval      b50opz=s1copz
038800020207      * campi chiave
038900051202     C                   move      *blanks       b50ke1
039000051202     C                   movel     s1cpou        b50ke1
039100051202     C                   move      *blanks       b50ke2
039200051202     c                   if        s1ccli<>*blanks
039300051202     C                   movel     s1ccli        b50ke2
039400051202     c                   endif
039500051202     c                   movel     s1hpoa        b50poa
039600051207     c                   movel     s1cres        b50ormreso
039700010924      *
039800020211      * Gestione richiamo tntb23r
039900051202     C                   EXSR      callb51r
040000020211      * ricarico il subfile
040100020211     C                   eval      $carsfl='1'
040200020211      * mi posiziono sul record da cui ho selezionato l'opzione
040300020211     C                   z-add     nrr           $nrr                           *posiziona in fondo
040400010924      *
040500020207     C                   else
040600010927      * c'e un errore sulle opzioni
040700020207     C                   seton                                        9082
040800020208     C                   seton                                        2842
040900020207     C                   exsr      ErrOpz
041000020207     C                   endif
041100020208     C                   endif
041200020207      *
041300020207      * rilegge sfile per sposizionamento
041400051202     C     nrr           chain     tb50s1                             97        *per sposizionamento
041500020207      *
041600020207      * aggiorna sfile
041700020207     C                   EXSR      aggsfl                                       *aggiorna sfile
041800020207      *
041900020207      * lettura successiva record
042000051202     C                   readc     tb50s1                                 82
042100010927     C                   enddo                                                  *fine sfile
042200020207      *
042300000000     C                   ENDSR
042400020207      *--------------------------------------------------------------------------------------------*
042500010927      * Errori su scelta opzione
042600020207      *--------------------------------------------------------------------------------------------*
042700010927     C     Erropz        BEGSR
042800020207      *
042900051202     C                   clear                   v1cmsg
043000020208      *
043100020207     C                   if        s1copz <>'2'  and s1copz <>'4'and
043200020207     C                             s1copz <>'5'  and s1copz <>'7'
043300051202     C                   eval      v1cmsg  = msg(1)
043400020207     C                   endif
043500010928      *
043600020207     C                   if        s1copz = '7'
043700051202     C                   eval      v1cmsg  = msg(2)
043800020207     C                   endif
043900010928      *
044000020207     C                   if        s1copz = '2'  or  s1copz = '4'
044100051202     C                   eval      v1cmsg  = msg(3)
044200020207     C                   endif
044300020207      *
044400020207     C                   ENDSR
044500020207      *--------------------------------------------------------------------------------------------*
044600020207      * aggsfl - aggiorna riga di sfile
044700020207      *--------------------------------------------------------------------------------------------*
044800000000     C     aggsfl        BEGSR
044900010924      * aggiorna sfile
045000020207     C                   Z-ADD     nrr           $nrr                           *posizionamento
045100020208     C                   seton                                        70
045200020208     C  n28              clear                   s1copz
045300011017      *
045400020215     C                   move      s1hflg        *in43
045500051202     C                   update    tb50s1
045600020207     C                   setoff                                       70
045700020207      *
045800000000     C                   ENDSR
045900020207      *--------------------------------------------------------------------------------------------*
046000020207      * RieS1  - vlorizzo i campi del subfile
046100020207      *--------------------------------------------------------------------------------------------*
046200020207     C     Ries1         BEGSR
046300020207      *
046400051202      * Se esiste cliente     decodifico
046500051202     C                   move      *blanks       s1dCLI
046600051202     C                   move      *blanks       s1cCLI
046700051205     C                   move      *blanks       s1dpo1
046800051205     C                   move      *blanks       s1dpo2
046900051202    1C     TBEKE2        ifne      *blanks
047000051202     C                   moveL     TBEKE2        $Cod              7 0
047100051202     C                   moveL     TBEKE2        S1CCLI
047200020208     C                   EXSR      DeCod
047300020208      *
047400051202    2C                   if        o69err = *blanks and acoflg =' '
047500051202     C                   movel     acorag        s1dCLI
047600051202    2C                   endif
047700051202    1C                   endif
047800020208      *
047900020208      * Punto operativo e decodifica
048000051206     C                   clear                   s1copz
048100051206     C                   movel     tbeke1        s1cpou
048200020214     C                   movel     *blanks       s1dpou
048300051202    1C     s1cpou        ifne      '999'
048400020207     C                   move      s1cpou        $Fil              3 0
048500020207     C     $Fil          chain     Azorg01l                           21
048600020207     C                   movel     orgdes        s1dpou
048700020214     C                   else
048800020214     C                   movel     'Tutti'       s1dpou
048900051202    1C                   endif
049000051202     c* p.o. abilitati
049100051206     c                   movel     tbeuni        d3tr
049200051206     c                   movea     §3trpoabi     skipo
049300051206     c                   movel     §3trpoabi     s1hpoa
049400051206     c                   movel     §3trporeso    s1cres
049500051206     c                   clear                   s1dres
049600051206     c                   if        s1cres>0
049700051206     C     s1cres        chain     Azorg01l                           21
049800051206     C  n21              movel     orgdes        s1dres
049900051206     c                   endif
050000051206     c
050100051202     C                   Z-ADD     1             XX                3 0
050200051202     C                   Z-ADD     1             YY                3 0
050300051202     C                   Z-ADD     1             ZZ                3 0
050400051202     C
050500051202    1C                   DOW       SKIPO(XX)>*zeros
050600051202     C                   MOVEL     SKIPO(XX)     W0030
050700051202    2C     W0030         ifne      999
050800051202     c
050900051202    3C                   IF        YY<17
051000051202     C                   EVAL      %SUBST(S1DPO1:YY:3)=skipo(xx)
051100051202     C                   ADD       4             YY
051200051202     C                   ELSE
051300051202     C                   EVAL      %SUBST(S1DPO2:zz:3)=skipo(xx)
051400051202     C                   ADD       4             zz
051500051202    3C                   ENDIf
051600051202     C
051700051202   x2C                   else
051800051202     C                   EVAL      S1DPO1=(skipo(xx) +'-'+ 'NESSUNO')
051900051202    2C                   endif
052000051202     c
052100051202     c                   add       1             xx
052200051202    1c                   enddo
052300010926      *
052400010921     C                   ENDSR
052500020207      *--------------------------------------------------------------------------------------------*
052600020207      * AtrS1  - Ripristino giusti attributi su sfl
052700020207      *--------------------------------------------------------------------------------------------*
052800011017     C     AtrS1         BEGSR
052900011017      *
053000020207      * record annullato
053100020207     C     tbeatb        ifeq      'A'
053200020207     C                   movel     'A'           S1CAnn
053300020208     C                   seton                                        43
053400020215     C                   move      *on           s1hflg
053500020207     C                   else
053600020207     C                   movel     ' '           S1CAnn
053700020208     C                   setoff                                       43
053800020215     C                   move      *off          s1hflg
053900020207     C                   endif
054000011017      *
054100020207     C                   ENDSR
054200020208      *--------------------------------------------------------------------------------------------*
054300020208      * DeCod  - Decodifica codice cliente
054400020208      *--------------------------------------------------------------------------------------------*
054500020208     C     DeCod         BEGSR
054600020208      *
054700020208     C                   clear                   tibs69ds
054800020208     C                   movel     $Kci          I69Kcc
054900020208     C                   movel     $Cod          I69Kac
055000020208     C                   movel     KnSif         I69Sif
055100020208     C                   call      'TIBS69R'
055200020208     C                   parm                    Tibs69Ds
055300020208     C                   parm                    DsAco
055400020208     C                   parm                    DsInd
055500020208     C                   parm                    DsClp
055600020208     C                   parm                    DsCls
055700020208      *
055800020208     C                   ENDSR
055900020207      *--------------------------------------------------------------------------------------------*
056000020207      * RutInz - operazioni iniziali
056100020207      *--------------------------------------------------------------------------------------------*
056200000210     C     rutinz        BEGSR
056300020207      *
056400020207      * Ricevimento parametri
056500000000     C     *ENTRY        PLIST
056600000000     C                   PARM                    kpjba
056700020207      *
056800020207     C                   z-add     1             CodUt
056900020207     C                   CALL      'X§PARUT'
057000020207     C                   PARM                    UT§DSE
057100020207      *
057200020207      * Recupero capoconto
057300020207      *
057400020207     C                   Do        50            X                 2 0
057500020207     C                   move      TCU(X)        TCUDS
057600020207     C     F56           ifeq      'CG'
057700020207     C     F4            andeq     '1'
057800020207     C     F3            andeq     '0'
057900020207     C                   z-add     KCU(X)        $Kci              4 0
058000020207     C                   leave
058100020207     C                   endif
058200020207     C                   Enddo
058300020207      *
058400020207      * Recupero S.I.
058500020207      *
058600020207     C                   MOVEL     *blanks       tbecod                         *tabella
058700020207     C                   MOVEL     *all'0'       tbeke1                         *chiave uno
058800051202     C                   MOVE      '3TR'         tbeke1
058900020207     C     k01tbe2       chain     tntbe01l                           97
059000020207     C  N97              movel     tbesif        $sif             10
059100051202     C  N97              movel     tbeapl        $apl              2
059200020207     C   97              move      'S'           $Fine
059300020207      *
059400020207      *
059500020207      * chiavi di lettura
059600020207     C     K01tbe2       KLIST                                                  *tntbe01l
059700020207     C                   KFLD                    tbecod                          -tabella
059800020207     C                   KFLD                    tbeke1                          -chiave uno
059900020207      *
060000051202     C     ktbe5         KLIST                                                  *tntbe01l
060100020207     C                   KFLD                    tbecod                          -tabella
060200020207     C                   KFLD                    tbelin                          -chiave uno
060300020207     C                   KFLD                    tbesif                          -chiave due
060400020207     C                   KFLD                    tbeke1                          -lingua
060500051202     C                   KFLD                    tbeke2                          -lingua
060600051202     C     ktbe4         KLIST                                                  *tntbe01l
060700051202     C                   KFLD                    tbecod                          -tabella
060800051202     C                   KFLD                    tbelin                          -chiave uno
060900051202     C                   KFLD                    tbesif                          -chiave due
061000051202     C                   KFLD                    tbeke1                          -lingua
061100051202     C     ktbe3         KLIST                                                  *tntbe01l
061200051202     C                   KFLD                    tbecod                          -tabella
061300051202     C                   KFLD                    tbelin                          -chiave uno
061400051202     C                   KFLD                    tbesif                          -chiave due
061500010924      *
061600020207     C                   ENDSR
061700020207      *--------------------------------------------------------------
061800010927** MSG  Lungh. 78                                                            *
061900020212Opzione non valida
062000020212Impossibile ripristinare un record già valido
062100020212Record annullato
062200051202Punto operativo errato
062300051202Immettere il codice cliente o il punto operativo
