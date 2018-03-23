000100970526     H*------------------------------------------------------------------------*
000200070119     H* Gestione anagrafica giri
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800070119     FFIDG09D   CF   E             WORKSTN INFDS(DEVDS1)
000900070119     F                                     SFILE(DG09D05:SFLULT)
001000070119     F                                     SFILE(DG09DMS:MSGULT)
001100070129     F                                     USROPN
001200970526     F*------------
001300070119     Ffigrg01l  IF   E           K DISK
001400070122     Ffigrg02l  IF   E           K DISK    rename(figrg000:figrg002)
001500071025     Ffigrg03l  IF   E           K DISK    rename(figrg000:figrg003)
001600070202     Ffidg09p   O    E             PRINTER OFLIND(*IN05) USROPN
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970528     D*------------
002900970528     D* COMANDI
003000970528     D*------------
003100970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970527     D FUV             S             19    DIM(24)
004800070905     D*------------
004900070905     D* Filiali gestibili
005000070905     D*------------
005100070905     D FIL             S              3  0 DIM(35)
005200970526     D*------------------------------------------------------------------------*
005300970526     D* INPUT
005400970526     D*------------------------------------------------------------------------*
005500970526     D*------------
005600970526     D* RIDEFINIZIONE OPZIONI VIDEO
005700970526     D*------------
005800970526     D OPDDS           DS
005900970526     D  OPDOPZ                 1      2
006000970526     D  OPDDE1                 3     19
006100970526     D  OPDOP1                21     22
006200970526     D  OPDFI0                23     34
006300970526     D  OPDATT                35     35
006400970526     D  OPDFI1                36     80
006500970526     D  OPDDES                 1     19
006600970526     D  OPDRIG                 1     80
006700970526     D*------------
006800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006900970526     D*------------
007000970526     D FUDDS           DS
007100970526     D  FUDDE1                 1      1
007200970526     D  FUDFUN                 2      3
007300970526     D  FUDDE2                 4     19
007400970526     D  FUDFI0                20     34
007500970526     D  FUDATT                35     35
007600970526     D  FUDFI1                36     80
007700970526     D  FUDDES                 1     19
007800970526     D  FUDRIG                 1     80
007900970526     D*------------
008000970526     D* DS GESTIONE TASTI FUNZIONALI
008100970526     D*------------
008200970526     D DSKEY         E DS
008300970526     D DEVDS1          DS
008400970526     D  KEY                  369    369
008500970526     D*------------
008600970526     D* DS "XMSG" - GESTIONE MESSAGGI
008700970526     D*------------
008800970526     D DSMSG         E DS
008900970526     D*------------
009000020916     D* DS REPERIMENTO DATI UTENTE
009100970526     D*------------
009200020916     D TIBS34DS      E DS                                                       *Profili utente
009300020916     D DDATIUTE      E DS                                                       *Dati utente
009400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009500070124     D*------------
009600070124     D* DS CONTROLLO FILIALI IN GESTIONE
009700070124     D*------------
009800170620     D**** FNLV50DS      E DS
009900970527     D*------------------
010000970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
010100970527     D*------------------
010200970527     D WLBDA8          DS                  INZ
010300970527     D  G08DAT                 1      8  0
010400970527     D  G08INV                 9     16  0
010500970527     D  G08ERR                17     17
010600970527     D  G08TGI                18     22  0
010700970526     D*------------
010800970526     D* DS DI PROCEDURA
010900001020     D*------------
011000070119     D fidg09ds      e ds
011100070320     D dgrgflo       e ds
011200070904      * - Reperimento terminal partenza/arrivo/£6
011300070904     D fnlv55ds      e ds
011400070904      * - Caricamento schiera filiali
011500070904     D trul06ds      e ds
011600070904     D  D06lin                 1     90  0 inz  dim(30)
011700970526     D*------------
011800970526     D* ARCHITETTURA
011900970526     D*------------
012000970526     D KPJBA         E DS
012100170620
012200170620     d sav_kpjbu       s                   like(kpjbu)
012300170612
012400170612      // - Reperimento filiali gestite dall'utente
012500170612     d TRUL31DS      e ds
012600170612     d  POG                   10    759    dim(250)
012700170612      //---------------------------------------------------------------
012800170612      //?Definizione Procedure usate.
012900170612      //---------------------------------------------------------------
013000170612      // - Caricamento Filiali in gestione
013100170612     d TRUL31R         pr                  extpgm('TRUL31R')
013200170612     d  kpjba                              likeds(kpjba)
013300170612     d  trul31ds                           likeds(trul31ds)
013400170612
013500970526     C*------------------------------------------------------------------------*
013600970526     C* MAIN LINES
013700970526     C*------------------------------------------------------------------------*
013800070124     C*
013900070124     C* SE NN RICHIESTA EMISSIONE VIDEO LEGGO E VERIFICO SOLO I DATI
014000070124     C                   IF        DEPOP0 = '001'
014100070124     C                   SETON                                        94
014200070124     C                   EXSR      CARSFL
014300070124     C                   IF        SFLRIG - 1= 1
014400070124     C                   EXSR      O01GES
014500070124     C                   ELSE
014600070124     C                   IF        SFLRIG > 2
014700070124     C                   MOVEL     '1'           d09OERR
014800070124     C                   MOVEL     MSG(7)        d09OMSG
014900070124     C                   ELSE
015000070124     C                   MOVEL     '1'           d09OERR
015100070124     C                   MOVEL     MSG(6)        d09OMSG
015200070124     C                   ENDIF
015300070124     C                   ENDIF
015400070124     C*
015500070124     C                   ELSE
015600070129     C*
015700070129     C* APRO IL FILE VIDEO
015800070129     C                   OPEN      FIDG09D
015900070124     C*
016000970526     C* SE NON CI SONO ERRORI, PROSEGUE
016100970526IF  1C     ERRL00        IFNE      '1'
016200070124     C*
016300070124     C                   SETOFF                                       94
016400970526     C*
016500970526     C* CARICA IL SFILE
016600970526     C                   EXSR      POSSFL
016700970526     C*
016800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
017000000703     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
017100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
017200970526     C*
017300970526     C* REIMPOSTA ERRORE DI PROCERURA
017400970526     C                   MOVEL     '0'           ERRL00
017500970526     C*
017600970526     C* EMETTE IL SFILE
017700970526     C                   EXSR      OUTSFL
017800970526     C*
017900970526     C* GESTIONE TASTI FUNZIONALI
018000970526     C                   EXSR      FUNCON
018100970526     C*
018200970526E   2C                   ENDDO
018300970527     C*
018400970527     C* CI SONO ERRORI IN ENTRATA
018500970527X   1C                   ELSE
018600070124     C                   MOVEL     '1'           d09OERR
018700070124     C                   MOVEL     DSMSMS        d09OMSG
018800970526E   1C                   ENDIF
018900070129     C*
019000070129     C* CHIUDO IL FILE VIDEO
019100070129     C                   CLOSE     FIDG09D
019200070124E   1C                   ENDIF
019300070124     C*
019400070124     C* RITORNO PARAMETRI
019500070124     C                   MOVEL(P)  fidg09ds      KPJBU
019600970526     C* FINE PGM
019700970526     C                   SETON                                        LR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F03GES        BEGSR
020200970526     C*
020300970526     C* IMPOSTA "PREMUTO F03"
020400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020500070124     C                   MOVEL     '1'           d09OF03                        *DI PROCEDURA
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F05GES        BEGSR
021200970526     C*
021300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021400970526     C*
021500970526     C                   ENDSR
021600070202     C*------------------------------------------------------------------------*
021700070202     C* F08GES - GESTIONE F08 -> STAMPA
021800070202     C*------------------------------------------------------------------------*
021900070202     C     F08GES        BEGSR
022000070202     C*
022100070202     C* Apro il prtf
022200070202     C                   open      fidg09p
022300070202     C*
022400070202     C* Stampo testata
022500070202     C                   write     dg09pte
022600070202     C                   write     dg09pt
022700070202     C                   write     dg09pri
022800070202     C                   setoff                                       05
022900070202     C*
023000070202     C                   SETON                                        60
023100070202     C                   EXSR      POSSFL                                       *RICARICA SFILE
023200070202     C                   SETOFF                                       60
023300070202     C*
023400070202     C* Stampo chiusura prospetto
023500070202     C                   write     dg09pri
023600070202     C                   write     dg09pbk
023700070202     C                   write     dg09pbk
023800070202     C                   write     dg09pfi
023900070202     C*
024000070202     C* Quindi chiudo il prtf
024100070202     C                   close     fidg09p
024200070202     C*
024300070202     C                   ENDSR
024400070119     C*------------------------------------------------------------------------*
024500070119     C* F10GES - GESTIONE F10 -> ORDINAMENTO X CODICE GIRO
024600070119     C*------------------------------------------------------------------------*
024700070119     C     F10GES        BEGSR
024800070119     C*
024900070123     C                   EVAL      WTIPRI = 'CGI'
025000070124     C                   Z-ADD     1             swmodi
025100070119     C                   EXSR      POSSFL                                       *RICARICA SFILE
025200070119     C*
025300070119     C                   ENDSR
025400070119     C*------------------------------------------------------------------------*
025500070119     C* F11GES - GESTIONE F11 -> ORDINAMENTO X DESCRIZIONE GIRO
025600070119     C*------------------------------------------------------------------------*
025700070119     C     F11GES        BEGSR
025800070119     C*
025900070119     C                   EVAL      WTIPRI = 'DES'
026000070124     C                   Z-ADD     1             swmodi
026100070119     C                   EXSR      POSSFL                                       *RICARICA SFILE
026200070119     C*
026300070119     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
026600970526     C*------------------------------------------------------------------------*
026700970526     C     F06GES        BEGSR
026800970526     C*
026900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027000070124     C                   Z-ADD     *ZEROS        d09idde
027100070124     C                   Z-ADD     *ZEROS        d09ifgs
027200070124     C                   MOVEL     *BLANKS       d09icgi
027300970602     C                   MOVEL     DEPOP0        WOP0
027400970529     C                   MOVEL     'F06'         WOP1
027500070119     C                   MOVEL     'FIDG10R'     WPGM              8
027600970526     C                   EXSR      LANPGM                                       *LANCIA PGM
027700070423     C                   Z-ADD     1             swmodi
027800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027900970526E   1C                   ENDIF
028000970526     C*
028100970526     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* F12GES - GESTIONE F12 -> RITORNO
028400970526     C*------------------------------------------------------------------------*
028500970526     C     F12GES        BEGSR
028600970526     C*
028700970526     C* IMPOSTA "PREMUTO F12"
028800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028900070124     C                   MOVEL     '1'           d09OF12                         *DI PROCEDURA
029000970526     C*
029100970526     C                   ENDSR
029200970528     C*------------------------------------------------------------------------*
029300070119     C* F21GES - GESTIONE F21 -> STAMPA ELENCO SECONDO SELEZIONI A VIDEO
029400970528     C*------------------------------------------------------------------------*
029500970528     C     F21GES        BEGSR
029600970612     C*
029700070124     C                   Z-ADD     *ZEROS        d09idde
029800070124     C                   Z-ADD     *ZEROS        d09ifgs
029900070124     C                   MOVEL     *BLANKS       d09icgi
030000970612     C                   MOVEL     DEPOP0        WOP0
030100970612     C                   MOVE      '03'          WOP0
030200970612     C                   MOVEL     'F21'         WOP1
030300070119     C                   MOVEL     'FIDG11R'     WPGM              8
030400070119     C                   EXSR      LANPGM
030500970528     C*
030600970528     C                   ENDSR
030700970528     C*------------------------------------------------------------------------*
030800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
030900970528     C*------------------------------------------------------------------------*
031000970528     C     F22GES        BEGSR
031100970528     C*
031200970528     C                   Z-ADD     80            LENGH            15 5
031300970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
031400970528     C                   PARM                    CMD(1)
031500970528     C                   PARM                    LENGH
031600970528     C*
031700970528     C                   ENDSR
031800970526     C*------------------------------------------------------------------------*
031900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
032000970526     C*------------------------------------------------------------------------*
032100970526     C     F23GES        BEGSR
032200970526     C*
032300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
032400970526     C*
032500970526     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
032800970526     C*------------------------------------------------------------------------*
032900970526     C     F24GES        BEGSR
033000970526     C*
033100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
033600970526     C*------------------------------------------------------------------------*
033700970526     C     RUPGES        BEGSR
033800970526     C*
033900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
034000970526     C*
034100970526     C                   ENDSR
034200070202     C*------------------------------------------------------------------------*
034300070202     C* STAMPA - Stampa giro corrente
034400070202     C*------------------------------------------------------------------------*
034500070202     C     STAMPA        BEGSR
034600070202     C*
034700070202     C* Valorizzo i dati in stampa
034800070202     C                   eval      stpatb   = grgatb
034900070202     C                   eval      stpfgs   = grgfgs
035000070202     C                   eval      stpgiro  = grgcgi
035100070202     C                   eval      stpgirod = grgdes
035200070202     C                   eval      stpain   = grgain
035300070202     C                   eval      stpdde   = viddde
035400070202     C                   eval      stpdscad = viddscad
035500070202     C                   if        grgact = 'N'
035600070202     C                   eval      stpact   = 'No'
035700070202     C                   else
035800070202     C                   eval      stpact   = 'Sì'
035900070202     C                   endif
036000070320     C                   eval      DGRGFLO = grgflo
036100070320     C                   if        §grgfit = 'S'
036200070320     C                   eval      stpfit   = 'Sì'
036300070320     C                   else
036400070320     C                   eval      stpfit   = 'No'
036500070320     C                   endif
036600070320     C                   if        §grgdst = 'N'
036700070320     C                   eval      stpdst   = 'No'
036800070320     C                   else
036900070320     C                   eval      stpdst   = 'Sì'
037000070320     C                   endif
037100070320     C                   if        §grgedp = 'S'
037200070320     C                   eval      stpedp   = 'Sì'
037300070320     C                   else
037400070320     C                   eval      stpedp   = 'No'
037500070320     C                   endif
037600110913     c                   clear                   stpztl
037700110913     c                   if        §grgcsto='S'
037800110913     c                   eval      stpztl='ZTL '
037900110913     c                   endif
038000120403     C                   eval      stpfgm   = §grgfgm
038100070202     C*
038200070202     C* Gestisco salto pagina
038300070202     C                   if        *in05 = *on
038400070202     C                   write     dg09pri
038500070202     C                   write     dg09pte
038600070202     C                   write     dg09pt
038700070202     C                   write     dg09pri
038800070202     C                   setoff                                       05
038900070202     C                   endif
039000070202     C*
039100070202     C* Quindi stampo giro corrente
039200070202     C                   write     dg09pd
039300070202     C*
039400070202     C                   ENDSR
039500970526     C*------------------------------------------------------------------------*
039600970526     C* ENTGES - GESTIONE ENTER -> INVIO
039700970526     C*------------------------------------------------------------------------*
039800970526     C     ENTGES        BEGSR
039900070124     C*
040000070124     C* CONTROLLO CHE IL P.O. INDICATO IN SELEZIONE SIA EFFETTIVAMENTE IN GESTIONE
040100070124     C* ALL'UTENTE CORRENTE
040200070124     C                   MOVEL     'S'           wAutPO            1
040300070601     C                   IF        %subst(KNMUS:1:3) <> 'EDP' AND
040400070605     C                             VI1FGS  = *ZEROS  and
040500070605      * se utente isp99 deve comportarsi come edp per i controlli del P.O. selezione
040600070605     C                             %subst(KNMUS:1:5) <> 'ISP99'
040700070601     C                   MOVEL     'N'           wAutPO
040800070601     C                   ENDIF
040900070124     C                   IF        DEPOP0 <> '001'  AND
041000070124     C                             DEPOP0 <> 'P01'  AND
041100070605     C                             %subst(KNMUS:1:3) <> 'EDP'  and
041200070605      * se utente isp99 deve comportarsi come edp per i controlli del P.O. selezione
041300070605     C                             %subst(KNMUS:1:5) <> 'ISP99'
041400170620     C******             CLEAR                   FNLV50DS
041500170620     C******             EVAL      D50TLA = 'L'
041600170620     C******             EVAL      D50PRU = KNMUS
041700170620     C******             EVAL      D50FGS = VI1FGS
041800170620     C******             CALL      'FNLV50R'
041900170620     C******             PARM                    FNLV50DS
042000170620     C******             IF        D50ERR <> *BLANKS
042100170620       //?Filiale in gestione all'utente
042200170620         IF  %lookup(%editc(vi1fgs:'X'):pog) = 0;
042300070124     C                   MOVEL     'N'           wAutPO
042400070124     C                   MOVEL     '1'           ERRL00
042500170620     C*****              MOVEL     D50MSG        DSMSMS
042600170620           dsmsms = 'Filiale in gestione errata';
042700070124     C                   ENDIF
042800070124     C                   ENDIF
042900070905      * se richiesto gestione delle filiali anche della £6 solo in '001' 'P01' verifico nella
043000070905      * schiera delle filiali
043100070905     C                   IF        (DEPOP0 = '001'  or
043200070905     C                             DEPOP0 = 'P01' ) AND
043300070905     C                             DEPf£6 = 'S'
043400070905      * verifico in schiera
043500070905     c     Vi1fgs        lookup    fil                                    30
043600070906     c                   If        not %found  or vi1fgs = 0
043700070905     C                   MOVEL     'N'           wAutPO
043800070905     C                   MOVEL     '1'           ERRL00
043900070905     C                   MOVEL     msg(8)        DSMSMS
044000070905     c                   endif
044100070905     c                   endif
044200070124     C*
044300070124     C                   IF        wAutPO = 'S'
044400000703     C*
044500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
044600970526     C                   EXSR      NEWPOS
044700970526     C*
044800970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
044900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045000970526IF  2C     SWPOSI        IFNE      0
045100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
045200970526X   2C                   ELSE
045300970526IF  3C     SFLULT        IFNE      0
045400970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
045500970526     C*
045600970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
045700970526IF  4C     SWMODI        IFNE      0
045800970526     C                   EXSR      POSSFL
045900970526E   4C                   ENDIF
046000970526E   3C                   ENDIF
046100970526E   2C                   ENDIF
046200970526E   1C                   ENDIF
046300970526     C*
046400070124     C                   ENDIF
046500070124     C*
046600970526     C                   ENDSR
046700000703     C*------------------------------------------------------------------------*
046800970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
046900970526     C*------------------------------------------------------------------------*
047000970526     C     O01GES        BEGSR
047100970526     C*
047200970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
047300070124     C                   Z-ADD     VIDFGS        d09ofgs
047400070124     C                   Z-ADD     VIHDDE        d09odde
047500070124     C                   Z-ADD     VIHDSCAD      d09odsc
047600070124     C                   MOVEL     VIDCGI        d09ocgi
047700070124     C                   MOVEL     VIDDES        d09odes
047800070124     C                   MOVEL     VIDAIN        d09oain
047900070124     C                   MOVEL     VIHATB        d09oatb
048000070124     C                   MOVEL     VIHACT        d09oact
048100070316     C                   MOVEL     VIHFLO        d09oflo
048200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
048300970526     C*
048400970526     C                   ENDSR
048500970526     C*------------------------------------------------------------------------*
048600970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
048700970526     C*------------------------------------------------------------------------*
048800970526     C     O02GES        BEGSR
048900970526     C*
049000970527     C* MODIFICA
049100970526IF  1C     ERRL00        IFEQ      '0'
049200070124     C                   Z-ADD     VIHDDE        d09idde
049300070124     C                   Z-ADD     VIDFGS        d09ifgs
049400070124     C                   MOVEL     VIDCGI        d09icgi
049500970602     C                   MOVEL     DEPOP0        WOP0
049600970526     C                   MOVEL     'O02'         WOP1
049700070119     C                   MOVEL     'FIDG10R'     WPGM
049800970526     C                   EXSR      LANPGM
049900070123     C                   Z-ADD     1             swmodi
050000970526E   1C                   ENDIF
050100970526     C*
050200970526     C                   ENDSR
050300991014     C*------------------------------------------------------------------------*
050400070119     C* O03GES - GESTIONE OPZIONE 3=COPIA
050500991014     C*------------------------------------------------------------------------*
050600991014     C     O03GES        BEGSR
050700991014     C*
050800070119     C* COPIA
050900991014IF  1C     ERRL00        IFEQ      '0'
051000070124     C                   Z-ADD     VIHDDE        d09idde
051100070124     C                   Z-ADD     VIDFGS        d09ifgs
051200070124     C                   MOVEL     VIDCGI        d09icgi
051300991014     C                   MOVEL     DEPOP0        WOP0
051400991014     C                   MOVEL     'O03'         WOP1
051500070119     C                   MOVEL     'FIDG10R'     WPGM
051600991014     C                   EXSR      LANPGM
051700070123     C                   Z-ADD     1             swmodi
051800991014E   1C                   ENDIF
051900991014     C*
052000991014     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
052300970526     C*------------------------------------------------------------------------*
052400070119     C     O04GES        BEGSR
052500070123     C*
052600070123     C* ANNULLAMENTO POSSIBILE SOLO SE RECORD NN GIA' ANNULLATO
052700070123     C                   IF        VIHATB <> *blanks
052800070123     C                   MOVEL     '1'           ERRL00
052900070123     C                   MOVEL     MSG(4)        DSMSMS
053000070123     C                   ELSE
053100070119     C*
053200970527     C* ANNULLAMENTO
053300070119IF  1C     ERRL00        IFEQ      '0'
053400070124     C                   Z-ADD     VIHDDE        d09idde
053500070124     C                   Z-ADD     VIDFGS        d09ifgs
053600070124     C                   MOVEL     VIDCGI        d09icgi
053700070119     C                   MOVEL     DEPOP0        WOP0
053800070119     C                   MOVEL     'O04'         WOP1
053900070119     C                   MOVEL     'FIDG10R'     WPGM
054000070119     C                   EXSR      LANPGM
054100070123     C                   Z-ADD     1             swmodi
054200070123     C                   SETON                                        71
054300070119E   1C                   ENDIF
054400070123     C                   ENDIF
054500970526     C*
054600070122     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
054900970526     C*------------------------------------------------------------------------*
055000970526     C     O05GES        BEGSR
055100970526     C*
055200070124     C                   Z-ADD     VIHDDE        d09idde
055300070124     C                   Z-ADD     VIDFGS        d09ifgs
055400070124     C                   MOVEL     VIDCGI        d09icgi
055500970602     C                   MOVEL     DEPOP0        WOP0
055600970526     C                   MOVEL     'O05'         WOP1
055700070119     C                   MOVEL     'FIDG10R'     WPGM
055800970526     C                   EXSR      LANPGM
055900070423     C                   Z-ADD     1             swmodi
056000970526     C*
056100970526     C                   ENDSR
056200970526     C*------------------------------------------------------------------------*
056300970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
056400970526     C*------------------------------------------------------------------------*
056500070123     C     O07GES        BEGSR
056600070123     C*
056700070123     C* RIPRISTINO POSSIBILE SOLO SE RECORD ANNULLATO
056800070123     C                   IF        VIHATB = *blanks
056900070123     C                   MOVEL     '1'           ERRL00
057000070123     C                   MOVEL     MSG(5)        DSMSMS
057100070123     C                   ELSE
057200970526     C*
057300970526     C* RIPRISITINO
057400070119IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
057500070124     C                   Z-ADD     VIHDDE        d09idde
057600070124     C                   Z-ADD     VIDFGS        d09ifgs
057700070124     C                   MOVEL     VIDCGI        d09icgi
057800070119     C                   MOVEL     DEPOP0        WOP0
057900070119     C                   MOVEL     'O07'         WOP1
058000070119     C                   MOVEL     'FIDG10R'     WPGM
058100070119     C                   EXSR      LANPGM
058200070123     C                   Z-ADD     1             SWMODI
058300070123     C                   SETOFF                                       71
058400070122E   1C                   ENDIF
058500070123     C                   ENDIF
058600970526     C*
058700970526     C                   ENDSR
058800970526     C*------------------------------------------------------------------------*
058900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
059000970526     C*------------------------------------------------------------------------*
059100970526     C     NEWPOS        BEGSR
059200970526     C*
059300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
059400970526     C*         1= RICHIESTO POSIZIONAMENTO
059500970526     C                   Z-ADD     0             SWPOSI            7 0
059600970526     C*
059700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
059800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
059900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
060000970526     C                   Z-ADD     0             SWMODI            1 0
060100970526     C*
060200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
060300070119IF  1C     VI1FGS        IFNE      depFGS
060400070123     C     VI1ATB        ORNE      depATB
060500070119     C     VI1AIN        ORNE      depAIN
060600070119     C     VI1DES        ORNE      depDES
060700070119     C     VI1DAT        ORNE      depDAT
060800070123     C     VI1CGI        ORNE      depCGI
060900110624     C     VI1csto       ORNE      depcsto
061000120403     C     VI1fgm        ORNE      depfgm
061100110624     C     VI1fit        ORNE      depfit
061200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
061300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
061400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
061500070119     C                   Z-ADD     VI1FGS        depFGS                         *DEPOSITI
061600070119     C                   MOVEL     VI1AIN        depAIN                         *DEPOSITI
061700070123     C                   MOVEL     VI1ATB        depATB                         *DEPOSITI
061800070119     C                   MOVEL     VI1DES        depDES                         *DEPOSITI
061900070119     C                   MOVEL     VI1DAT        depDAT                         *DEPOSITI
062000070123     C                   MOVEL     VI1CGI        depCGI                         *DEPOSITI
062100110624     C                   MOVEL     VI1csto       depCsto
062200120403     C                   MOVEL     VI1fgm        depfgm
062300110624     C                   MOVEL     VI1fit        depfit
062400970526E   2C                   ENDIF
062500970526E   1C                   ENDIF
062600970526     C*
062700970526     C                   ENDSR
062800970526     C*------------------------------------------------------------------------*
062900970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
063000970526     C*------------------------------------------------------------------------*
063100970528     C     SETFIL        BEGSR
063200970526     C*
063300970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
063400070123IF  1C     WTIPRI        IFEQ      'CGI'                                        *PER CODICE GIRO
063500070123     C     *loval        SETLL     figrg01l
063600970528E   1C                   ENDIF
063700070119     C*
063800070119IF  1C     WTIPRI        IFEQ      'DES'                                        *PER DESCRIZ. GIRO
063900070123     C     *loval        SETLL     figrg02l
064000070119E   1C                   ENDIF
064100071025     C*
064200071025IF  1C     WTIPRI        IFEQ      'NOV'                                        *PER FIL/GIRO
064300071025     C                   IF        DEPOP0='001'
064400071025     C     KEY_GRG03_P   SETLL     figrg03l
064500071025     C                   ELSE
064600071025     C     *loval        SETLL     figrg03l
064700071025     C                   ENDIF
064800071025E   1C                   ENDIF
064900980107     C*
065000970526     C* LETTURA RECORD
065100970526IF  1C     *IN99         IFEQ      *OFF
065200970526     C                   EXSR      LETFIL
065300970526E   1C                   ENDIF
065400970526     C*
065500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
065600970526IF  1C     *IN99         IFEQ      *ON
065700970526     C                   Z-ADD     1             SFLEOF
065800970526E   1C                   ENDIF
065900970526     C*
066000970526     C                   ENDSR
066100970526     C*------------------------------------------------------------------------*
066200970526     C* LETFIL - LETTURA RECORD
066300970526     C*------------------------------------------------------------------------*
066400970526     C     LETFIL        BEGSR
066500970526     C*
066600970526     C* LEGGE FINO A:
066700970526     C*     - FINE FILE
066800970526     C*     - TROVATO RECORD VALIDO
066900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
067000010306     C     *IN99         DOWEQ     *OFF
067100970526     C     WRECOK        ANDEQ     'N'
067200970526     C*
067300970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
067400070123     C     WTIPRI        IFEQ      'CGI'                                        *PER CODICE GIRO
067500070119     C                   READ      figrg01l                               99
067600010306     C                   ENDIF
067700970528     C*
067800070119     C     WTIPRI        IFEQ      'DES'                                        *PER DESCRIZ. GIRO
067900070119     C                   READ      figrg02l                               99
068000070119     C                   ENDIF
068100071025     C*
068200071025     C     WTIPRI        IFEQ      'NOV'                                        *PER FIL/GIRO
068300071025     C                   READ      figrg03l                               99
068400071025     C                   ENDIF
068500070119     C*
068600970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
068700010306     C     *IN99         IFEQ      *ON                                          *FINE FILE
068800970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
068900010306     C                   ELSE
069000071025     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
069100071025     C*
069200071025     C* Se passata chiave in input => esco dalla lettura
069300071025     C                   if        DEPOP0 = '001'
069400071025     C                   if        grgFGS <> D09IFGS AND
069500071025     C                             grgCGI <> D09ICGI
069600071025     C                   movel     '1'           F03L00
069700071025     C                   movel     '1'           SFLEOF
069800071025     C                   seton                                        99
069900071025     C                   endif
070000071025     C                   endif
070100071025     C*
070200010306     C                   ENDIF
070300010306     C                   ENDDO
070400970526     C*
070500970526     C                   ENDSR
070600970526     C*------------------------------------------------------------------------*
070700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
070800970526     C*------------------------------------------------------------------------*
070900970526     C     CHKREC        BEGSR
071000970526     C*
071100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
071200070622     C                   MOVEL     GRGFLO        DGRGFLO
071300010117     C*
071400070119     C* FILTRO X P.O. GESTIONE
071500010427     C                   if        WRECOK = 'S'
071600070119     C     VI1FGS        ifeq      *zeros
071700010117     C                   else
071800070119     C                   if        grgfgs <> VI1FGS
071900010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
072000010117     C                   endif
072100070119     C                   endif
072200010427     C                   endif
072300120403     C* FILTRO X giro micro
072400120403     C                   if        WRECOK = 'S'
072500120403     C     VI1fgm        ifeq      ' '
072600120403     C                   else
072700120403     C                   if        §grgfgm <> VI1FGm
072800120403     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
072900120403     C                   endif
073000120403     C                   endif
073100120403     C                   endif
073200070119     C*
073300070119     C* FILTRO X AMBITO
073400070119     C                   if        WRECOK = 'S'
073500070119     C     VI1AIN        ifeq      *blanks
073600070119     C                   else
073700070119     C                   if        grgain <> VI1AIN
073800070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
073900070119     C                   endif
074000070119     C                   endif
074100070119     C                   endif
074200070119     C*
074300070119     C* FILTRO X ALLA DATA
074400070119     C                   if        WRECOK = 'S'
074500070123     C     VI1DAT        ifeq      *zeros
074600070123     C                   else
074700070123     C*
074800070123     C                   z-add     VI1DAT        G08DAT
074900070123     C                   z-add     *ZEROS        G08INV
075000070123     C                   movel     '0'           G08ERR
075100070123     C                   call      'XSRDA8'
075200070123     C                   parm                    WLBDA8
075300070123     C                   z-add     G08INV        wvi1dat
075400070123     C*
075500070119     C                   if        wVI1DAT >= grgdde AND
075600070124     C                             wVI1DAT <= grgdscad
075700070119     C                   else
075800070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
075900070119     C                   endif
076000070119     C                   endif
076100070119     C                   endif
076200010427     C*
076300070119     C* FILTRA X DESCRIZIONE GIRO (IN "LIKE")
076400010427     C                   if        WRECOK = 'S'
076500070119     C     VI1DES        ifeq      *blanks
076600010709     C                   else
076700010709     C                   z-add     *zeros        pos               2 0
076800070119     C                   eval      pos = %scan(%trim(VI1DES):grgdes)
076900010709     C                   if        pos = *zeros
077000010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
077100010427     C                   endif
077200010427     C                   endif
077300010427     C                   endif
077400070119     C*
077500070119     C* FILTRO X FLAG ANNULLAMENTO
077600070119     C                   if        WRECOK = 'S'
077700070123     C                   if        VI1ATB <> *blanks
077800070122     C                   if        VI1ATB <> grgatb
077900070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
078000070119     C                   endif
078100070123     C                   endif
078200070119     C                   endif
078300070122     C*
078400070122     C* FILTRO X CODICE GIRO
078500070122     C                   if        WRECOK = 'S'
078600070123     C     VI1CGI        ifeq      *blanks
078700070122     C                   else
078800070123     C                   if        grgcgi <> VI1CGI
078900070122     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079000070122     C                   endif
079100070122     C                   endif
079200070122     C                   endif
079300070411     C*
079400070411     C* FILTRO X DATA DECORRENZA (SOLO DA *PGM CHIAMANTE)
079500070411     C                   if        WRECOK = 'S'
079600070411     C                   if        d09idde <> *zeros
079700070411     C                   if        d09idde <> grgdde
079800070411     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079900070411     C                   endif
080000070411     C                   endif
080100070411     C                   endif
080200070622     C*
080300070622     C* FILTRO X tipo utilizzo giro
080400070622     C                   if        WRECOK = 'S'
080500070703     C                   if        D09ITUG = *BLANK                             NESSUNA SELEZIONE
080600070622     C                   else
080700070703      * se tipo utilizzo del giro non è uguale alla selezione fatta oppure
080800070703      * non è tipo utilizzo E=entrambi il record non è valido
080900070703     C                   if        §grgTUG <> D09itug and §grgTUG <>'E'
081000070622     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081100070622     C                   endif
081200070622     C                   endif
081300070622     C                   endif
081400110624     C* FILTRO X fittizio
081500110624     C                   if        WRECOK = 'S'
081600110624     C                   if        (§grgfit = ' ' and vi1fit='S') or
081700110624     C                             (§grgfit = 'S' and vi1fit='N')
081800110624     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081900110624     C                   endif
082000110624     C                   endif
082100110624     c* giri centro storico
082200110624     C                   if        WRECOK = 'S'
082300110624     C                   if        §grgcsto= ' ' and vi1csto='S'
082400110624     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
082500110624     C                   endif
082600110624     C                   endif
082700970526     C*
082800970526     C                   ENDSR
082900970526     C*------------------------------------------------------------------------*
083000970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
083100970526     C*------------------------------------------------------------------------*
083200970526     C     IRESFL        BEGSR
083300070119     C*
083400070119     C                   setoff                                       71
083500970526     C*
083600070119     C                   movel     *BLANKS       OPZKEY
083700070123     C                   movel     grgcgi        VIDCGI
083800070119     C                   movel     grgdes        VIDDES
083900070123     C                   movel     grgain        VIDAIN
084000070119     C                   movel     grgatb        VIHATB
084100070119     C                   movel     grgact        VIHACT
084200070316     C                   movel     grgflo        VIHFLO
084300070119     C                   z-add     grgdde        VIHDDE
084400070124     C                   z-add     grgdscad      VIHDSCAD
084500070119     C                   z-add     grgfgs        VIDFGS
084600070119     C*
084700070119     C* Se giro annullato metto in reverse il codice giro stesso
084800070119     C                   if        grgatb <> *blanks
084900070119     C                   seton                                        71
085000070119     C                   endif
085100110624     c
085200110624     c                   clear                   vidtip
085300110624     c                   movel     grgflo        dgrgflo
085400110624     c
085500110624     c* Visulizzo se giro centro storico o fittizio
085600110624     c                   select
085700110624     c                   when      §grgcsto='S' and §grgfit='S'
085800110913     c                   eval      vidtip='ZT/F'
085900110624     c
086000110624     c                   when      §grgcsto='S'
086100110913     c                   eval      vidtip='ZTL '
086200110624     c
086300110624     c                   when      §grgfit='S'
086400110913     c                   eval      vidtip='Fit '
086500110624     c                   endsl
086600110624     c
086700070119     C* Inverto a video data decorrenza
086800070119     C                   Z-ADD     grgdde        G08INV
086900991025     C                   Z-ADD     *ZEROS        G08DAT
087000991025     C                   MOVEL     '3'           G08ERR
087100991025     C                   CALL      'XSRDA8'
087200991025     C                   PARM                    WLBDA8
087300070119     C                   Z-ADD     G08DAT        VIDDDE
087400070119     C*
087500070119     C* Inverto a video data scadenza
087600070124     C                   Z-ADD     grgdscad      G08INV
087700991025     C                   Z-ADD     *ZEROS        G08DAT
087800991025     C                   MOVEL     '3'           G08ERR
087900991025     C                   CALL      'XSRDA8'
088000991025     C                   PARM                    WLBDA8
088100070124     C                   Z-ADD     G08DAT        VIDDSCAD
088200070202     C*
088300070202     C* Gestisco se richiesta la stampa dei giri (secondo selezione corrente)
088400070202     C   60              exsr      stampa
088500011129     C*
088600970526     C                   ENDSR
088700970526     C*------------------------------------------------------------------------*
088800970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
088900970526     C*------------------------------------------------------------------------*
089000070119     C     CHKPAR        BEGSR
089100970528     C*
089200000801     C*
089300970526     C                   ENDSR
089400970526     C*------------------------------------------------------------------------*
089500970526     C* LANPGM - LANCIA UN PROGRAMMA
089600970526     C*------------------------------------------------------------------------*
089700970526     C     LANPGM        BEGSR
089800970526     C*
089900970526     C* IMPOSTA LA DS DI PROCEDURA
090000070124     C                   MOVEL     WOP0          d09IOP0                        *LIVELLO PROCEDURA
090100070124     C                   MOVEL     WOP1          d09IOP1                        *LIVELLO PGM
090200070124     C                   MOVEL     '0'           d09OF03                        *PREMUTO F03=FINE
090300070124     C                   MOVEL     '0'           d09OF12                        *PREMUTO F12=RITORNO
090400070124     C                   MOVEL     '0'           d09OERR                        *'1'=ERRORE
090500070124     C                   MOVEL     *BLANKS       d09OMSG                        *MESSAGGIO DI RITORN
090600070123     C                   MOVEL(P)  fidg09ds      KPJBU
090700000705     C*
090800970526     C                   CALL      WPGM
090900970612     C                   PARM                    KPJBA
091000070123     C                   MOVEL(P)  KPJBU         fidg09ds
091100970526     C*
091200970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
091300070124IF  1C     d09OF03       IFEQ      '1'                                          *PREMUTO F03
091400070122     C                   MOVEL     '1'           ERRL00                         *ERRORE
091500970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
091600970526X   1C                   ELSE                                                   *NON PREMUTO F03
091700070124     C                   MOVEL     d09OMSG       DSMSMS                        *MESSAGGIO DI RITORN
091800970526E   1C                   ENDIF
091900970526     C*
092000970526     C                   ENDSR
092100970526     C*------------------------------------------------------------------------*
092200070119     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
092300970526     C*------------------------------------------------------------------------*
092400970526     C     CHKFOP        BEGSR
092500070122     C*
092600070122     C                   MOVEL     FUD(10)       FUDRIG                         *ORDINAMENTO
092700070122     C                   MOVEL     'S'           FUDATT
092800070122     C                   MOVEL     FUDRIG        FUD(10)
092900070122     C                   MOVEL     FUD(11)       FUDRIG                         *ORDINAMENTO
093000070122     C                   MOVEL     'S'           FUDATT
093100070122     C                   MOVEL     FUDRIG        FUD(11)
093200970626     C*
093300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
093400970626IF  1C     DEPOP0        IFEQ      'P01'
093500970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
093600970626     C                   MOVEL     'S'           OPDATT
093700970626     C                   MOVEL     OPDRIG        OPD(1)
093800970626X   1C                   ELSE
093900970626     C* OPZIONI
094000970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
094100970626     C                   MOVEL     'S'           OPDATT
094200970626     C                   MOVEL     OPDRIG        OPD(5)
094300970626     C* FUNZIONI
094400970626E   1C                   ENDIF
094500970626     C*
094600970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
094700070122IF  1C     DEPOP0        IFEQ      'P03'
094800070122     C     DEPOP0        OREQ      'P04'
094900970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
095000970626     C                   MOVEL     'S'           OPDATT
095100970626     C                   MOVEL     OPDRIG        OPD(2)
095200070122     C***                MOVEL     OPD(3)        OPDRIG                         *COPIA
095300070122     C***                MOVEL     'S'           OPDATT
095400070122     C***                MOVEL     OPDRIG        OPD(3)
095500070601     C*
095600070601     C* ANNULLAMENTO CONSENTITO SOLO A PERSONALE EDP*
095700070601     C                   IF        %subst(KNMUS:1:3)  = 'EDP'
095800070119     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
095900070119     C                   MOVEL     'S'           OPDATT
096000070119     C                   MOVEL     OPDRIG        OPD(4)
096100070601     C                   ENDIF
096200070601     C*
096300070119     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
096400070119     C                   MOVEL     'S'           OPDATT
096500070119     C                   MOVEL     OPDRIG        OPD(7)
096600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
096700970626     C                   MOVEL     'S'           FUDATT
096800970626     C                   MOVEL     FUDRIG        FUD(6)
096900970626E   1C                   ENDIF
097000070119     C*
097100070124     C* SE IN SCELTA INOLTRE DISABILITO ALCUNE SELEZIONI A VIDEO
097200070904     C                   SETON                                        7092
097300070124IF  1C     DEPOP0        IFEQ      'P01'
097400070904     C                   SETOFF                                       70
097500070124     C                   SETOFF                                       93
097600070904      * se non devo impostare la filiale di gestione
097700070904     c                   If        depf£6 = ' '
097800070904     c                   setoff                                       92
097900070904     c                   endif
098000070904
098100070119E   1C                   ENDIF
098200970526     C*
098300970526     C* GESTIONE OPZIONI A VIDEO
098400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
098500970526     C                   EXSR      OPZGES
098600970526     C*
098700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
098800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
098900970526     C                   EXSR      FUNGES
099000970526     C*
099100970526     C                   ENDSR
099200020916     C*--------------------------------------------------------------------------------------------*
099300070119     C* REPERISCE I DATI UTENTE
099400020916     C*--------------------------------------------------------------------------------------------*
099500020916     C     REPDATIUTE    BEGSR
099600020916     C*
099700020916     C* INIZIALIZZA VARIABILI DI WRK
099800020916     C                   CLEAR                   TIBS34DS
099900020916     C                   CLEAR                   AZUTEDS
100000020916     C                   CLEAR                   DDATIUTE
100100020916     C*
100200020916     C     *DTAARA       DEFINE    §azute        azuteds
100300020916     C     *DTAARA       DEFINE    §datiute      ddatiute
100400020916     C                   IN(E)     *DTAARA
100500020916if  1C                   IF        %Error
100600020916     c                   EVAL      I34Tla = 'L'
100700020916     C                   CALL      'TIBS34R'
100800020916     C                   PARM                    Tibs34Ds
100900020916     C                   IN        *DTAARA
101000020916e   1C                   ENDIF
101100020916     C*
101200020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101300020916     C                   MOVEL(P)  RSUT          DSFIRS           20
101400170612
101500170612      // Reperimento delle filiali gestibili dall'utente
101600170620        sav_kpjbu = kpjbu;
101700170612        clear TRUL31DS;
101800170620        IF  DUTlpo = '1';
101900170620          I31abi = 'RC';
102000170620        ELSE;
102100170620          I31abi = 'PO';
102200170620        ENDIF;
102300170612        I31cdi = DUTdis;
102400170612        I31car = DUTare;
102500170612        I31cpo = DUTpou;
102600170612        trul31r (kpjba:trul31ds);
102700170612        IF  O31pog <= *zeros;
102800170612          clear POG;
102900170612        ENDIF;
103000170620        kpjbu = sav_kpjbu;
103100020916     C*
103200020916     C                   ENDSR
103300970526     C*------------------------------------------------------------------------*
103400970526     C* *INZSR - OPERAZIONI INIZIALI
103500970526     C*------------------------------------------------------------------------*
103600970526     C     *INZSR        BEGSR
103700970526     C*---
103800970526     C* RICEVIMENTO PARAMETRI
103900970526     C*---
104000970526     C     *ENTRY        PLIST
104100970612     C                   PARM                    KPJBA
104200071025     C*
104300071025     C* CHIAVI DI ACCESSO
104400071025     C*
104500071025     C* FIGRG03L - PARZIALE
104600071025     C     KEY_GRG03_P   KLIST
104700071025     C                   KFLD                    D09IFGS
104800071025     C                   KFLD                    D09ICGI
104900071025     C*
105000070905     C                   TIME                    W0140            14 0
105100070905      * UDATE IN GGMMAAAA
105200070905     C                   MOVE      W0140         oggi_ggmmaa       8 0
105300070905      * UDATE IN AAAAMMGG
105400070905     C                   Z-ADD     oggi_ggmmaa   G08DAT
105500070905     C                   MOVEL     *BLANK        G08ERR
105600070905     C                   CALL      'XSRDA8'
105700070905     C                   PARM                    WLBDA8
105800070905     C                   Z-ADD     G08INV        oggi_aammgg       8 0
105900070306     C*
106000070306     C* REPERIMENTO DATI UTENTE
106100070306     C                   EXSR      REPDATIUTE
106200070122     C*
106300070125     C                   IF        %subst(KNMUS:1:3) = 'EDP'
106400070125     C                   SETON                                        93
106500070125     C                   ELSE
106600070124     C                   SETOFF                                       93
106700070125     C                   ENDIF
106800070124     C*
106900070905
107000070905     c                   clear                   fil
107100070905
107200070122     C                   IF        KPJBU <> *blanks
107300070119     C                   MOVEL     KPJBU         fidg09ds
107400070122     C*
107500070122     C* DEPOSITA LE OPZIONI DI ENTRATA
107600070124     C                   MOVEL     d09IOP0       DEPOP0                         *LIVELLO PROCEDURA
107700070124     C                   MOVEL     d09IOP1       DEPOP1                         *LIVELLO PROGRAMMA
107800070904     C                   MOVEL     d09IF£6       DEPf£6                         *Richiesta fil gest
107900070122     C*
108000070122     C* IMPOSTA I PARAMETRI DI INGRESSO
108100070124     C                   MOVE(P)   d09ifgs       VI1FGS
108200070124     C                   MOVE(P)   d09ifgs       depFGS
108300070124     C                   Z-ADD     d09idat       wVI1DAT
108400070124     C                   MOVEL(P)  d09icgi       VI1CGI
108500070124     C                   MOVEL(P)  d09icgi       depCGI
108600070124     C                   MOVEL(P)  d09ides       VI1DES
108700070124     C                   MOVEL(P)  d09ides       depDES
108800070124     C                   MOVEL(P)  d09iain       VI1AIN
108900070124     C                   MOVEL(P)  d09iain       depAIN
109000070124     C                   MOVEL(P)  d09iatb       VI1ATB
109100070124     C                   MOVEL(P)  d09iatb       depATB
109200070905      * se flag richiesta filiale di gestione è diverso da blank recupero le filiali
109300070905     c                   If        depf£6 =  'S'
109400070905     c                   exsr      recfil
109500070905     c                   endif
109600070405     C*
109700070405     C* Se nn rischiesta emissione file video forzo i parametri d selezione anche nei
109800070405     C* campi del video
109900070405     C                   IF        DEPOP0 = '001'
110000070405     C                   MOVE(P)   d09ifgs       VI1FGS
110100070405     C                   MOVEL(P)  d09icgi       VI1CGI
110200070405     C                   MOVEL(P)  d09ides       VI1DES
110300070405     C                   MOVEL(P)  d09iain       VI1AIN
110400070405     C                   MOVEL(P)  d09iatb       VI1ATB
110500070405     C                   Z-ADD     d09idat       wVI1DAT
110600070405     C                   ENDIF
110700070122     C*
110800070122     C* Inverto la data "validi al"
110900070122     C                   Z-ADD     wVI1DAT       G08INV
111000070122     C                   Z-ADD     *ZEROS        G08DAT
111100070122     C                   MOVEL     '3'           G08ERR
111200070122     C                   CALL      'XSRDA8'
111300070122     C                   PARM                    WLBDA8
111400070122     C                   Z-ADD     G08DAT        VI1DAT
111500070124     C                   Z-ADD     G08DAT        depDAT
111600070122     C*
111700070122     C                   EXSR      CHKPAR
111800070122IF  1C     ERRL00        IFEQ      '1'
111900070124     C                   MOVEL     '1'           d09OERR
112000070124     C                   MOVEL     DSMSMS        d09OMSG
112100070122E   1C                   ENDIF
112200070122     C*
112300070122     C* SE ENTRATA DA MENU' GESTISCO IN BASE ALL'UTENTE
112400070122     C                   ELSE
112500070122     C                   CLEAR                   fidg09ds
112600070122     C                   IF        %subst(KNMUS:1:3) = 'EDP'
112700070122     C                   EVAL      DEPOP0 = 'P04'
112800070122     C                   ELSE
112900070306     C                   EVAL      VI1FGS = DUTPOU
113000070124     C                   EVAL      DEPOP0 = 'P03'
113100070122     C                   ENDIF
113200070122     C                   ENDIF
113300970526     C*
113400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
113500970526     C     XMSGPA        PLIST
113600970526     C                   PARM                    DSMSMS
113700970526     C                   PARM                    DSMSPG
113800970526     C                   PARM                    DSMSKE
113900970526     C                   PARM                    DSMSER
114000970526     C*
114100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
114200970526     C                   CALL      'XKEY'
114300970526     C                   PARM                    DSKEY
114400070119     C*
114500070119     C* DI DEFAULT IMPOSTO L?ORDINAMENTO X CODICE GIRO
114600070123     C                   MOVEL     'CGI'         WTIPRI            3
114700071025     C                   IF        DEPOP0 = '001'
114800071025     C                   MOVEL     'NOV'         WTIPRI
114900071025     C                   ENDIF
115000970526     C*---
115100970526     C* VARIABILI NON RIFERITE AL DATA BASE
115200970526     C*---
115300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
115400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
115500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
115600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
115700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
115800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
115900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
116000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
116100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
116200070124     C     *LIKE         DEFINE    d09IOP0       DEPOP0                         *DI DEPOSITO
116300070124     C     *LIKE         DEFINE    d09IOP1       DEPOP1
116400070904     C     *LIKE         DEFINE    d09IF£6       DEPF£6
116500070124     C     *LIKE         DEFINE    d09IOP0       WOP0                           *DI LAVORO
116600070124     C     *LIKE         DEFINE    d09IOP1       WOP1
116700970526     C*---
116800110624     C     *LIKE         DEFINE    §grgcsto      Depcsto
116900120403     C     *LIKE         DEFINE    §grgfgm       Depfgm
117000110624     C     *LIKE         DEFINE    §grgfit       Depfit
117100110624     C     *LIKE         DEFINE    d09iain       DepAIN
117200070124     C     *LIKE         DEFINE    d09iatb       DepATB
117300070124     C     *LIKE         DEFINE    d09ides       DepDES
117400070124     C     *LIKE         DEFINE    d09idat       DepDAT
117500070124     C     *LIKE         DEFINE    d09idat       wVI1DAT
117600070124     C     *LIKE         DEFINE    d09icgi       DepCGI
117700070124     C     *LIKE         DEFINE    d09ifgs       DepFGS
117800001020     C*---
117900970526     C* VARIABILI DI CONTROLLO
118000970526     C*---
118100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
118200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
118300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
118400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
118500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
118600070201     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
118700970526     C*
118800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
118900070119     C                   MOVEL     'FIDG09R'     DSMSPG
119000070119     C                   MOVEL     'FIDG09R'     MSGQUE
119100970526     C                   MOVEL     *BLANKS       MSGKEY
119200970526     C                   MOVEL     *BLANKS       DSMSMS
119300970526     C                   MOVEL     *BLANKS       DSMSKE
119400970526     C                   Z-ADD     *ZEROS        DSMSER
119500970526     C*
119600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
119700970526     C                   EXSR      CHKFOP
119800970526     C*
119900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
120000970530IF  1C     DEPOP0        IFEQ      'P01'
120100070119     C                   MOVEL     DEC(1)        VIDING
120200970530E   1C                   ENDIF
120300970530IF  1C     DEPOP0        IFEQ      'P02'
120400070119     C                   MOVEL     DEC(2)        VIDING
120500970530E   1C                   ENDIF
120600070119IF  1C     DEPOP0        IFEQ      'P03'
120700070119     C                   MOVEL     DEC(3)        VIDING
120800070119E   1C                   ENDIF
120900070122IF  1C     DEPOP0        IFEQ      'P04'
121000070122     C                   MOVEL     DEC(4)        VIDING
121100970526E   1C                   ENDIF
121200970526     C*
121300070119     C                   ENDSR
121400070905      *********************************************************************
121500070905      * Recupero £6 e capofila
121600070905      *********************************************************************
121700070905     c     Recfil        begsr
121800070905
121900070905     c                   clear                   fil
122000070904      * Reperimento capofila £6
122100070905     c                   clear                   FNLV55ds
122200070904     c                   eval      D55tpt  =  '6'
122300070904     c                   eval      D55lin  =  VI1FGS
122400070905     c                   eval      D55drf  =  oggi_aammgg
122500070904     c                   call      'FNLV55R'
122600070904     c                   parm                    FNLV55ds
122700070904if  2c                   if        D55err  <> *blanks
122800070904     c                   eval      D55tfa  =  VI1FGS
122900070904e   2c                   endif
123000070905     c                   eval      fil(1) = d55tfa
123100070905
123200070904      * Reperimento filiali gestione Arrivi (£6)
123300070905
123400070905     c                   clear                   TRUL06ds
123500070905     c                   eval      D06cod   = '£6'
123600070905     c                   movel(p)  Vi1fgs        D06key
123700070905     c                   movel(p)  TRUL06ds      KPJBU
123800070905     c                   call      'TRUL06R'
123900070905     c                   parm                    KPJBA
124000070905     c                   movel     KPJBU         TRUL06ds
124100070905     c                   movea     D06lin        fil(2)
124200070905
124300070904     c                   endsr
124400970526     C*------------------------------------------------------------------------*
124500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
124800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
124900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
125000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
125100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
125200970526     C*------------------------------------------------------------------------*
125300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
125400970526     C*------------------------------------------------------------------------*
125500970526     C     POSSFL        BEGSR
125600970526     C*
125700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
125800970526     C                   EXSR      CLRSFL
125900970526     C*
126000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
126100970526     C                   EXSR      SETFIL
126200970526     C*
126300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
126400970526     C     SFLEOF        IFEQ      0
126500970526     C                   EXSR      CARSFL
126600970526     C                   END
126700970526     C*
126800970526     C                   ENDSR
126900970526     C*------------------------------------------------------------------------*
127000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
127100970526     C*------------------------------------------------------------------------*
127200970526     C     ENTATT        BEGSR
127300970526     C*
127400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
127500970526     C                   Z-ADD     0             OPZOK             1 0
127600970526     C*
127700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
127800970526     C                   Z-ADD     0             SFLVAR            5 0
127900970526     C*
128000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
128100970526     C                   Z-ADD     0             SFLERR
128200970526     C*
128300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
128400970526     C                   Z-ADD     SFLULT        WFLULT
128500970526     C*
128600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
128700070119     C                   READC     DG09D05                                98
128800970526     C     *IN98         DOWEQ     '0'
128900970526     C*
129000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
129100970526     C                   ADD       1             SFLVAR
129200970526     C*
129300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
129400970526     C     OPZOK         IFEQ      0
129500970526     C*
129600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
129700970526     C     OPZKEY        IFNE      *BLANKS
129800970526     C                   EXSR      OPZCON
129900970526     C                   END
130000970526     C*
130100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
130200970526     C*            2. PULIZIA SCELTA IN SFL
130300970526     C     OPZOK         IFEQ      0
130400970526     C                   MOVEL     '0'           *IN41
130500970526     C                   MOVEL     *BLANKS       OPZKEY
130600970526     C                   ELSE
130700970526     C*
130800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
130900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
131000970526     C                   MOVEL     '1'           *IN41
131100970526     C                   Z-ADD     SFLULT        SFLERR
131200970526     C                   END
131300970526     C                   ELSE
131400970526     C*
131500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
131600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
131700970526     C                   MOVEL     '1'           *IN41
131800970526     C                   END                                                    OPZOK=0
131900970526     C*
132000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
132100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
132200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
132300970526     C     OPZOK         IFNE      0
132400970526     C                   Z-ADD     SFLERR        SFLNBR
132500970526     C                   ELSE
132600970526     C                   Z-ADD     SFLULT        SFLNBR
132700970526     C                   END
132800970526     C*
132900970526     C* AGGIORNO RECORD SFL
133000070906     C*
133100070906     C* Se giro annullato metto in reverse il codice giro stesso
133200070906     C                   if        vihatb <> *blanks
133300070906     C                   seton                                        71
133400070906     C                   endif
133500070906     C*
133600070119     C                   UPDATE    DG09D05
133700970526     C*
133800070119     C                   READC     DG09D05                                98
133900970526     C                   END                                                    DO WHILE
134000970526     C*
134100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
134200970526     C                   Z-ADD     WFLULT        SFLULT
134300970526     C*
134400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
134500970526     C     SFLVAR        IFEQ      0
134600970526     C*---                 MOVEL'1'       FINL00
134700970526     C                   END
134800970526     C*
134900970526     C                   ENDSR
135000970526     C*------------------------------------------------------------------------*
135100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
135200970526     C*------------------------------------------------------------------------*
135300970526     C     CLRSFL        BEGSR
135400970526     C*
135500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
135600970526     C* (0=NON FINE FILE  1=FINE FILE)
135700970526     C                   Z-ADD     0             SFLEOF
135800970526     C*
135900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
136000970526     C                   Z-ADD     0             SFLULT
136100970526     C*
136200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
136300970526     C                   MOVEL     '1'           *IN40
136400070119     C                   WRITE     DG09D06
136500970526     C                   MOVEL     '0'           *IN40
136600970526     C*
136700970526     C                   ENDSR
136800970526     C*------------------------------------------------------------------------*
136900970526     C* CARSFL - CARICAMENTO SFL
137000970526     C*------------------------------------------------------------------------*
137100970526     C     CARSFL        BEGSR
137200970526     C*
137300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
137400970526     C     SFLULT        IFEQ      0
137500970526     C                   EXSR      SETFIL
137600970526     C                   END
137700970526     C*
137800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
137900970526     C                   Z-ADD     SFLULT        WFLULT
138000970526     C*
138100970526     C* CICLO:
138200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
138300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
138400970526     C                   Z-ADD     1             SFLRIG
138500070205     C**** SFLRIG        DOWLE     SFLPAG
138600070205     C**** SFLEOF        ANDEQ     0
138700070205     C                   DOW       (SFLRIG <= SFLPAG OR *IN60 = *ON) AND
138800070205     C                             SFLEOF  = *ZEROS
138900970526     C*
139000970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
139100970526     C                   MOVEL     *BLANKS       OPZKEY
139200970526     C*
139300970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
139400070124     C                   EXSR      IRESFL
139500970526     C*
139600970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
139700120403     c                   if        sflult < 9999
139800970526     C                   ADD       1             SFLULT
139900970526     C* SCRITTURA RECORD SFL
140000070124     C  N94              WRITE     DG09D05
140100120403     C                   ADD       1             SFLRIG
140200120403     c                   end
140300970526     C*
140400970526     C* LETTURA FILE DA CARICARE
140500970526     C                   EXSR      LETFIL
140600970526     C                   END                                                    DO WHILE
140700970526     C*
140800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
140900970526     C     WFLULT        IFNE      SFLULT
141000970526     C                   Z-ADD     WFLULT        SFLNBR
141100970526     C                   ADD       1             SFLNBR
141200970526     C                   END
141300970526     C*
141400970526     C                   ENDSR
141500970526     C*------------------------------------------------------------------------*
141600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
141700970526     C*------------------------------------------------------------------------*
141800970526     C     OUTSFL        BEGSR
141900970526     C*
142000970526     C* WRITE RECORD TESTATA VIDEO
142100070119     C                   WRITE     DG09D01
142200970526     C*
142300970526     C* VISUALIZZAZIONE MESSAGGIO
142400970526     C                   EXSR      SNDMSG
142500970526     C*
142600970526     C* WRITE RECORD OPZIONI VIDEO
142700070119     C                   WRITE     DG09D02
142800970526     C*
142900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
143000070119     C                   WRITE     DG09D03
143100970526     C*
143200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
143300070119     C                   WRITE     DG09D04
143400970526     C*
143500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
143600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
143700970526     C     SFLEOF        IFEQ      0
143800970526     C                   MOVEL     '0'           *IN42
143900970526     C                   ELSE
144000970526     C*
144100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
144200970526     C                   MOVEL     '1'           *IN42
144300970526     C                   END
144400970526     C*
144500970526     C* WRITE RECORD TASTI FUNZIONALI
144600070119     C                   WRITE     DG09D09
144700970526     C*
144800970526     C* CASO DI SFL VUOTO
144900970526     C     SFLULT        IFLE      0
145000970526     C*
145100970526     C*    - WRITE RECORD VUOTO
145200070323     C                   WRITE     DG09D0A
145300070119     C                   READ      DG09D0A                                99
145400070323     C*    - TRACCIO NN TROVATO NESSUN DATO RISPONDENTE A RICHIESTE
145500070323     C                   MOVEL     '1'           d09OERR
145600070323     C                   MOVEL     MSG(6)        d09OMSG
145700970526     C*
145800970526     C                   ELSE
145900970526     C*
146000970526     C* CASO DI SFL CON RECORDS
146100970526     C*
146200970526     C*    - WRITE RECORD CONTROLLO SFL
146300070119     C                   WRITE     DG09D06
146400070119     C                   READ      DG09D06                                99
146500970526     C                   END
146600970526     C*
146700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
146800070119     C                   READ      DG09D03                                99
146900970526     C*
147000970526     C                   ENDSR
147100970526     C*------------------------------------------------------------------------*
147200970526     C* SNDMSG - MANDA MESSAGGIO
147300970526     C*------------------------------------------------------------------------*
147400970526     C     SNDMSG        BEGSR
147500970526     C*
147600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
147700970526     C                   MOVEL     '1'           *IN40
147800070119     C                   WRITE     DG09DMC
147900970526     C                   MOVEL     '0'           *IN40
148000970526     C*
148100970526     C* VISUALIZZAZIONE MESSAGGIO
148200970526     C                   Z-ADD     0             MSGULT
148300970526     C                   CALL      'XMSG'        XMSGPA
148400970526     C     DSMSER        IFEQ      0
148500970526     C                   MOVEL     DSMSKE        MSGKEY
148600970526     C                   END
148700970526     C                   Z-ADD     1             MSGULT
148800070119     C                   WRITE     DG09DMS                                      SFL MESS
148900070119     C                   WRITE     DG09DMC                                      CONTR MESS
149000970526     C*
149100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
149200070905     C                   MOVEL     *BLANKS       DSMSMS
149300970526     C*
149400970526     C                   ENDSR
149500970526     C*------------------------------------------------------------------------*
149600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
149700970526     C*------------------------------------------------------------------------*
149800970526     C     FUNCON        BEGSR
149900970526     C*
150000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
150100970526     C                   EXSR      FUNVAR
150200970526     C*
150300970526     C* PULISCO CAMPI DI LAVORO
150400970526     C                   Z-ADD     0             I                 5 0
150500970526     C                   Z-ADD     0             W                 5 0
150600970526     C*
150700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
150800970526     C     FUNKEY        IFNE      *BLANKS
150900970526     C     1             DO        24            I
151000970526     C                   MOVEL     FUD(I)        FUDRIG
151100970526     C*
151200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
151300970526     C     FUNKEY        IFEQ      FUDFUN
151400970526     C     FUDATT        ANDEQ     'S'
151500970526     C                   Z-ADD     I             W
151600970526     C                   END
151700970526     C                   END                                                    DO WHILE
151800970526     C*
151900970526     C* FUNZIONE NON ATTIVA (ERRORE)
152000970526     C     W             IFEQ      0
152100970526     C*
152200970526     C* EMISSIONE MESSAGGIO DI ERRORE
152300970526     C                   MOVEL     MSG(2)        DSMSMS
152400970526     C                   END                                                    W=0
152500970526     C                   END                                                    FUNKEY NE *BLANKS
152600970526     C*
152700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
152800970526     C     W             IFNE      0
152900970526     C     FUNKEY        OREQ      *BLANKS
153000970526     C*
153100970526     C     FUNKEY        IFEQ      *BLANKS
153200970526     C                   EXSR      ENTGES
153300970526     C                   END
153400970526     C     FUNKEY        IFEQ      '03'
153500970526     C                   EXSR      F03GES
153600970526     C                   END
153700970526     C     FUNKEY        IFEQ      '05'
153800970526     C                   EXSR      F05GES
153900970526     C                   END
154000970526     C     FUNKEY        IFEQ      '06'
154100970526     C                   EXSR      F06GES
154200970526     C                   END
154300070202     C     FUNKEY        IFEQ      '08'
154400070202     C                   EXSR      F08GES
154500070202     C                   END
154600070119     C     FUNKEY        IFEQ      '10'
154700070119     C                   EXSR      F10GES
154800070119     C                   END
154900070119     C     FUNKEY        IFEQ      '11'
155000070119     C                   EXSR      F11GES
155100070119     C                   END
155200970526     C     FUNKEY        IFEQ      '12'
155300970526     C                   EXSR      F12GES
155400970526     C                   END
155500970528     C     FUNKEY        IFEQ      '21'
155600970528     C                   EXSR      F21GES
155700970528     C                   END
155800970528     C     FUNKEY        IFEQ      '22'
155900970528     C                   EXSR      F22GES
156000970528     C                   END
156100970526     C     FUNKEY        IFEQ      '23'
156200970526     C                   EXSR      F23GES
156300970526     C                   END
156400970526     C     FUNKEY        IFEQ      '24'
156500970526     C                   EXSR      F24GES
156600970526     C                   END
156700970526     C     FUNKEY        IFEQ      'RU'
156800970526     C                   EXSR      RUPGES
156900970526     C                   END
157000970526     C*
157100970526     C                   END
157200970526     C*
157300970526     C                   ENDSR
157400970526     C*------------------------------------------------------------------------*
157500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
157600970526     C*------------------------------------------------------------------------*
157700970526     C     FUNVAR        BEGSR
157800970526     C*
157900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
158000970526     C     KEY           IFEQ      ENTER
158100970526     C                   MOVEL     *BLANKS       FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F1
158400970526     C                   MOVEL     '01'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F2
158700970526     C                   MOVEL     '02'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F3
159000970526     C                   MOVEL     '03'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F4
159300970526     C                   MOVEL     '04'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F5
159600970526     C                   MOVEL     '05'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F6
159900970526     C                   MOVEL     '06'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F7
160200970526     C                   MOVEL     '07'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F8
160500970526     C                   MOVEL     '08'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F9
160800970526     C                   MOVEL     '09'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F10
161100970526     C                   MOVEL     '10'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F11
161400970526     C                   MOVEL     '11'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F12
161700970526     C                   MOVEL     '12'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F13
162000970526     C                   MOVEL     '13'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F14
162300970526     C                   MOVEL     '14'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F15
162600970526     C                   MOVEL     '15'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      F16
162900970526     C                   MOVEL     '16'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      F17
163200970526     C                   MOVEL     '17'          FUNKEY
163300970526     C                   END
163400970526     C     KEY           IFEQ      F18
163500970526     C                   MOVEL     '18'          FUNKEY
163600970526     C                   END
163700970526     C     KEY           IFEQ      F19
163800970526     C                   MOVEL     '19'          FUNKEY
163900970526     C                   END
164000970526     C     KEY           IFEQ      F20
164100970526     C                   MOVEL     '20'          FUNKEY
164200970526     C                   END
164300970526     C     KEY           IFEQ      F21
164400970526     C                   MOVEL     '21'          FUNKEY
164500970526     C                   END
164600970526     C     KEY           IFEQ      F22
164700970526     C                   MOVEL     '22'          FUNKEY
164800970526     C                   END
164900970526     C     KEY           IFEQ      F23
165000970526     C                   MOVEL     '23'          FUNKEY
165100970526     C                   END
165200970526     C     KEY           IFEQ      F24
165300970526     C                   MOVEL     '24'          FUNKEY
165400970526     C                   END
165500970526     C     KEY           IFEQ      ROLLUP
165600970526     C                   MOVEL     'RU'          FUNKEY
165700970526     C                   END
165800970526     C     KEY           IFEQ      HELP
165900970526     C                   MOVEL     'HP'          FUNKEY
166000970526     C                   END
166100970526     C*
166200970526     C                   ENDSR
166300970526     C*------------------------------------------------------------------------*
166400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
166500970526     C*------------------------------------------------------------------------*
166600970526     C     FUNGES        BEGSR
166700970526     C*
166800970526     C* PULISCO CAMPI DI LAVORO
166900970526     C                   Z-ADD     0             W                 5 0
167000970526     C*
167100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
167200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
167300970526     C     1             DO        24            I
167400970526     C                   MOVEL     FUD(I)        FUDRIG
167500970526     C*
167600970526     C* PULIZIA FUNZIONI PRECEDENTI
167700970526     C                   MOVEL     *BLANKS       FUV(I)
167800970526     C*
167900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
168000970526     C* (SALTA F23 E F24)
168100970526     C     FUDATT        IFEQ      'S'
168200970526     C     FUDFUN        ANDNE     '23'
168300970526     C     FUDFUN        ANDNE     '24'
168400970526     C                   ADD       1             W
168500970526     C                   MOVEL     FUDDES        FUV(W)
168600970526     C                   END
168700970526     C                   END                                                    DO WHILE
168800970526     C*
168900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
169000970526     C* ALTRIMENTI DISATTIVO F24
169100970526     C     1             DO        24            I
169200970526     C                   MOVEL     FUD(I)        FUDRIG
169300970526     C*
169400970526     C     FUDFUN        IFEQ      '24'
169500970526     C     W             IFGT      6
169600970526     C                   MOVEL     'S'           FUDATT
169700970526     C                   ELSE
169800970526     C                   MOVEL     *BLANKS       FUDATT
169900970526     C                   END                                                    W GT 8
170000970526     C                   MOVEL     FUDRIG        FUD(I)
170100970526     C                   END
170200970526     C                   END                                                    DO WHILE
170300970526     C*
170400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
170500970526     C     W             DIV       6             I
170600970526     C                   MVR                     Z
170700970526     C*
170800970526     C     Z             IFNE      0
170900970526     C     I             ADD       1             Z
171000970526     C                   ELSE
171100970526     C                   Z-ADD     I             Z
171200970526     C                   END
171300970526     C*
171400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
171500970526     C                   ADD       1             FUNGRU
171600970526     C     FUNGRU        IFGT      Z
171700970526     C     FUNGRU        ORLT      1
171800970526     C                   Z-ADD     1             FUNGRU
171900970526     C                   END
172000970526     C*
172100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
172200970526     C     FUNGRU        IFEQ      1
172300970526     C                   MOVEL     FUV(1)        FUN001
172400970526     C                   MOVEL     FUV(2)        FUN002
172500970526     C                   MOVEL     FUV(3)        FUN003
172600970526     C                   MOVEL     FUV(4)        FUN004
172700970526     C                   MOVEL     FUV(5)        FUN005
172800970526     C                   MOVEL     FUV(6)        FUN006
172900970526     C                   END
173000970526     C*
173100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
173200970526     C     FUNGRU        IFEQ      2
173300970526     C                   MOVEL     FUV(7)        FUN001
173400970526     C                   MOVEL     FUV(8)        FUN002
173500970526     C                   MOVEL     FUV(9)        FUN003
173600970526     C                   MOVEL     FUV(10)       FUN004
173700970526     C                   MOVEL     FUV(11)       FUN005
173800970526     C                   MOVEL     FUV(12)       FUN006
173900970526     C                   END
174000970526     C*
174100970526     C* TERZO GRUPPO DI 6 FUNZIONI
174200970526     C     FUNGRU        IFEQ      3
174300970526     C                   MOVEL     FUV(13)       FUN001
174400970526     C                   MOVEL     FUV(14)       FUN002
174500970526     C                   MOVEL     FUV(15)       FUN003
174600970526     C                   MOVEL     FUV(16)       FUN004
174700970526     C                   MOVEL     FUV(17)       FUN005
174800970526     C                   MOVEL     FUV(18)       FUN006
174900970526     C                   END
175000970526     C*
175100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
175200970526     C     FUNGRU        IFEQ      4
175300970526     C                   MOVEL     FUV(19)       FUN001
175400970526     C                   MOVEL     FUV(20)       FUN002
175500970526     C                   MOVEL     FUV(21)       FUN003
175600970526     C                   MOVEL     FUV(22)       FUN004
175700970526     C                   MOVEL     *BLANKS       FUN005
175800970526     C                   MOVEL     *BLANKS       FUN006
175900970526     C                   END
176000970526     C*
176100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
176200970526     C                   MOVEL     *BLANKS       FUN007
176300970526     C                   MOVEL     *BLANKS       FUN008
176400970526     C*
176500970526     C     1             DO        24            I
176600970526     C                   MOVEL     FUD(I)        FUDRIG
176700970526     C     FUDFUN        IFEQ      '23'
176800970526     C     FUDATT        ANDEQ     'S'
176900970526     C                   MOVEL     FUDDES        FUN007
177000970526     C                   END
177100970526     C     FUDFUN        IFEQ      '24'
177200970526     C     FUDATT        ANDEQ     'S'
177300970526     C                   MOVEL     FUDDES        FUN008
177400970526     C                   END
177500970526     C                   END                                                    DO
177600970526     C*
177700970526     C                   ENDSR
177800970526     C*------------------------------------------------------------------------*
177900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
178000970526     C*------------------------------------------------------------------------*
178100970526     C     OPZGES        BEGSR
178200970526     C*
178300970526     C* PULISCO CAMPI DI LAVORO
178400970526     C                   Z-ADD     0             W                 5 0
178500970526     C*
178600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
178700970526     C* E CARICO TABELLA OPZIONI ATTIVE
178800970526     C     1             DO        24            I
178900970526     C                   MOVEL     OPD(I)        OPDRIG
179000970526     C*
179100970526     C* PULIZIA OPZIONI PRECEDENTI
179200970526     C                   MOVEL     *BLANKS       OPV(I)
179300970526     C*
179400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
179500970526     C     OPDATT        IFEQ      'S'
179600970526     C                   ADD       1             W
179700970526     C                   MOVEL     OPDDES        OPV(W)
179800970526     C                   END
179900970526     C                   END                                                    DO WHILE
180000970526     C*
180100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
180200970526     C* ALTRIMENTI DISATTIVO F23
180300970526     C     1             DO        24            I
180400970526     C                   MOVEL     FUD(I)        FUDRIG
180500970526     C*
180600970526     C     FUDFUN        IFEQ      '23'
180700970526     C     W             IFGT      8
180800970526     C                   MOVEL     'S'           FUDATT
180900970526     C                   ELSE
181000970526     C                   MOVEL     *BLANKS       FUDATT
181100970526     C                   END                                                    W GT 8
181200970526     C                   MOVEL     FUDRIG        FUD(I)
181300970526     C                   END
181400970526     C                   END                                                    DO WHILE
181500970526     C*
181600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
181700970526     C     W             DIV       8             I
181800970526     C                   MVR                     Z                 5 0
181900970526     C*
182000970526     C     Z             IFNE      0
182100970526     C     I             ADD       1             Z
182200970526     C                   ELSE
182300970526     C                   Z-ADD     I             Z
182400970526     C                   END
182500970526     C*
182600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
182700970526     C                   ADD       1             OPZGRU
182800970526     C     OPZGRU        IFGT      Z
182900970526     C     OPZGRU        ORLT      1
183000970526     C                   Z-ADD     1             OPZGRU
183100970526     C                   END
183200970526     C*
183300970526     C* PRIMO GRUPPO DI 8 OPZIONI
183400970526     C     OPZGRU        IFEQ      1
183500970526     C                   MOVEL     OPV(1)        OPZ001
183600970526     C                   MOVEL     OPV(2)        OPZ002
183700970526     C                   MOVEL     OPV(3)        OPZ003
183800970526     C                   MOVEL     OPV(4)        OPZ004
183900970526     C                   MOVEL     OPV(5)        OPZ005
184000970526     C                   MOVEL     OPV(6)        OPZ006
184100970526     C                   MOVEL     OPV(7)        OPZ007
184200970526     C                   MOVEL     OPV(8)        OPZ008
184300970526     C                   END
184400970526     C*
184500970526     C* SECONDO GRUPPO DI 8 OPZIONI
184600970526     C     OPZGRU        IFEQ      2
184700970526     C                   MOVEL     OPV(9)        OPZ001
184800970526     C                   MOVEL     OPV(10)       OPZ002
184900970526     C                   MOVEL     OPV(11)       OPZ003
185000970526     C                   MOVEL     OPV(12)       OPZ004
185100970526     C                   MOVEL     OPV(13)       OPZ005
185200970526     C                   MOVEL     OPV(14)       OPZ006
185300970526     C                   MOVEL     OPV(15)       OPZ007
185400970526     C                   MOVEL     OPV(16)       OPZ008
185500970526     C                   END
185600970526     C*
185700970526     C* TERZO GRUPPO DI 8 OPZIONI
185800970526     C     OPZGRU        IFEQ      3
185900970526     C                   MOVEL     OPV(17)       OPZ001
186000970526     C                   MOVEL     OPV(18)       OPZ002
186100970526     C                   MOVEL     OPV(19)       OPZ003
186200970526     C                   MOVEL     OPV(20)       OPZ004
186300970526     C                   MOVEL     OPV(21)       OPZ005
186400970526     C                   MOVEL     OPV(22)       OPZ006
186500970526     C                   MOVEL     OPV(23)       OPZ007
186600970526     C                   MOVEL     OPV(24)       OPZ008
186700970526     C                   END
186800970526     C*
186900970526     C                   ENDSR
187000970526     C*------------------------------------------------------------------------*
187100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
187200970526     C*------------------------------------------------------------------------*
187300970526     C     OPZCON        BEGSR
187400970526     C*
187500970526     C* PULISCO CAMPI DI LAVORO
187600970526     C                   Z-ADD     0             I                 5 0
187700970526     C                   Z-ADD     0             W                 5 0
187800970526     C*
187900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
188000970526     C     1             DO        24            I
188100970526     C                   MOVEL     OPD(I)        OPDRIG
188200970526     C*
188300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
188400970526     C     OPZKEY        IFEQ      OPDOPZ
188500970526     C     OPZKEY        OREQ      OPDOP1
188600970526     C*
188700970526     C     OPDATT        IFEQ      'S'
188800970526     C                   MOVEL     OPDOPZ        OPZKEY
188900970526     C                   Z-ADD     I             W
189000970526     C                   END
189100970526     C                   END
189200970526     C*
189300970526     C                   END                                                    DO WHILE
189400970526     C*
189500970526     C* OPZIONE NON ATTIVA (ERRORE)
189600970526     C     W             IFEQ      0
189700970526     C*
189800970526     C* OPZIONE NON OK
189900970526     C                   Z-ADD     1             OPZOK
190000970526     C*
190100970526     C* EMISSIONE MESSAGGIO DI ERRORE
190200970526     C                   MOVEL     MSG(1)        DSMSMS
190300970526     C                   ELSE
190400970526     C*
190500970526     C* OPZIONE ATTIVA (PROCEDI ...)
190600970526     C*
190700970526     C     OPZKEY        IFEQ      ' 1'
190800970526     C                   EXSR      O01GES
190900970526     C                   END
191000970526     C     OPZKEY        IFEQ      ' 2'
191100970526     C                   EXSR      O02GES
191200970526     C                   END
191300991014     C     OPZKEY        IFEQ      ' 3'
191400991014     C                   EXSR      O03GES
191500991014     C                   END
191600070119     C     OPZKEY        IFEQ      ' 4'
191700070119     C                   EXSR      O04GES
191800070119     C                   END
191900970526     C     OPZKEY        IFEQ      ' 5'
192000970526     C                   EXSR      O05GES
192100970526     C                   END
192200970526     C     OPZKEY        IFEQ      ' 7'
192300970526     C                   EXSR      O07GES
192400970526     C                   END
192500970526     C                   END                                                    OPZ.NON ATTIVA
192600970526     C*
192700970526     C                   ENDSR
192800970526     C*------------------------------------------------------------------------*
192900970526     C* NOTE COSTRUZIONE SCHIERE
193000970526     C*------------------------------------------------------------------------*
193100970526     C*
193200970526     C* -MSG-MESSAGGI
193300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
193400970526     C* (NON TOCCARE MSG,1 E MSG,2)
193500970526     C*
193600970526     C* -CMD-COMANDI
193700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
193800970526     C*
193900970526     C* -OPD-OPZIONI
194000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
194100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
194200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
194300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
194400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
194500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
194600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
194700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
194800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
194900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
195000970526     C*
195100970526     C* -FUD-FUNZIONI
195200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
195300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
195400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
195500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
195600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
195700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
195800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
195900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
196000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
196100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
196200970526     O*------------------------------------------------------------------------*
196300970526** DEC - DECODIFICHE
196400070119     SCELTA                                                      1
196500070122 INTERROGAZIONE                                                  2
196600070122    GESTIONE                                                     3
196700070122  GESTIONE FULL                                                  4
196800070122                                                                 5
196900970526** MSG - MESSAGGI
197000070119FIDG09R- Opzione non gestita                                                   1
197100070119FIDG09R- Tasto funzionale non gestito                                          2
197200070119FIDG09R- 3                                                                     3
197300070119FIDG09R- Record annullato. Consentito solo il ripristino                       4
197400070119FIDG09R- Record non annullato. Impossibile ripristinare                        5
197500070124FIDG09R- Nessun record corrispondente ai requisiti richiesti                   6
197600070124FIDG09R- Trovati troppi record corrispondenti ai requisiti richiesti           7
197700070905FIDG09R- Filiale errata                                                        8
197800070119FIDG09R- 9                                                                     9
197900070119FIDG09R- 10                                                                   10
198000070119FIDG09R- 11                                                                   11
198100070119FIDG09R- Data errata                                                          12
198200070119FIDG09R- 13                                                                   13
198300070119FIDG09R- 14                                                                   14
198400070119FIDG09R- 15                                                                   15
198500070119FIDG09R- 16                                                                   16
198600070119FIDG09R- 17                                                                   17
198700070119FIDG09R- 18                                                                   18
198800070119FIDG09R- 19                                                                   19
198900070119FIDG09R- 20                                                                   20
199000970528** CMD - COMANDI CL
199100970528WRKSPLF
199200970528
199300970528
199400970528
199500970528
199600970526** OPD - OPZIONI
199700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
199800970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
199900991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
200000070119 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
200100970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
200200000703                   -6 -     (S/N)=N                        opz. 06¬
200300970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
200400970527                   -8 -     (S/N)=N                        opz. 08¬
200500070119                   -9 -     (S/N)=N                        opz. 09¬
200600070119                   -10-     (S/N)=N                        opz. 10¬
200700070119                   -11-     (S/N)=N                        opz. 11¬
200800970526                   -  -     (S/N)=N                        opz. 12¬
200900970526                   -  -     (S/N)=N                        opz. 13¬
201000970526                   -  -     (S/N)=N                        opz. 14¬
201100970526                   -  -     (S/N)=N                        opz. 15¬
201200970526                   -  -     (S/N)=N                        opz. 16¬
201300970526                   -  -     (S/N)=N                        opz. 17¬
201400970526                   -  -     (S/N)=N                        opz. 18¬
201500970526                   -  -     (S/N)=N                        opz. 19¬
201600970526                   -  -     (S/N)=N                        opz. 20¬
201700970526                   -  -     (S/N)=N                        opz. 21¬
201800970526                   -  -     (S/N)=N                        opz. 22¬
201900970526                   -  -     (S/N)=N                        opz. 23¬
202000970526                   -  -     (S/N)=N                        opz. 24¬
202100970526** FUD - FUNZIONI
202200970526                   -        (S/N)=N                        funz.01¬
202300970526                   -        (S/N)=N                        funz.02¬
202400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
202500970526                   -        (S/N)=N                        funz.04¬
202600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
202700970526F06=Inserimento    -        (S/N)=N                        funz.06¬
202800970526                   -        (S/N)=N                        funz.07¬
202900070205F08=Stampa elenco  -        (S/N)=S  <== S-FISSO           funz.08¬
203000970527                   -        (S/N)=N                        funz.09¬
203100070119F10=Ordina x cod.  -        (S/N)=S                        funz.10¬
203200070119F11=Ordina x desc. -        (S/N)=S                        funz.11¬
203300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
203400970526                   -        (S/N)=N                        funz.13¬
203500970526                   -        (S/N)=N                        funz.14¬
203600970526                   -        (S/N)=N                        funz.15¬
203700970526                   -        (S/N)=N                        funz.16¬
203800970526                   -        (S/N)=N                        funz.17¬
203900970526                   -        (S/N)=N                        funz.18¬
204000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
204100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
204200070205F21=Stampa elenco  -        (S/N)=N                        funz.21¬
204300970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
204400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
204500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
