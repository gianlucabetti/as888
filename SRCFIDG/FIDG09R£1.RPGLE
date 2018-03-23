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
009800070124     D FNLV50DS      E DS
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
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400070124     C*
012500070124     C* SE NN RICHIESTA EMISSIONE VIDEO LEGGO E VERIFICO SOLO I DATI
012600070124     C                   IF        DEPOP0 = '001'
012700070124     C                   SETON                                        94
012800070124     C                   EXSR      CARSFL
012900070124     C                   IF        SFLRIG - 1= 1
013000070124     C                   EXSR      O01GES
013100070124     C                   ELSE
013200070124     C                   IF        SFLRIG > 2
013300070124     C                   MOVEL     '1'           d09OERR
013400070124     C                   MOVEL     MSG(7)        d09OMSG
013500070124     C                   ELSE
013600070124     C                   MOVEL     '1'           d09OERR
013700070124     C                   MOVEL     MSG(6)        d09OMSG
013800070124     C                   ENDIF
013900070124     C                   ENDIF
014000070124     C*
014100070124     C                   ELSE
014200070129     C*
014300070129     C* APRO IL FILE VIDEO
014400070129     C                   OPEN      FIDG09D
014500070124     C*
014600970526     C* SE NON CI SONO ERRORI, PROSEGUE
014700970526IF  1C     ERRL00        IFNE      '1'
014800070124     C*
014900070124     C                   SETOFF                                       94
015000970526     C*
015100970526     C* CARICA IL SFILE
015200970526     C                   EXSR      POSSFL
015300970526     C*
015400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015600000703     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015800970526     C*
015900970526     C* REIMPOSTA ERRORE DI PROCERURA
016000970526     C                   MOVEL     '0'           ERRL00
016100970526     C*
016200970526     C* EMETTE IL SFILE
016300970526     C                   EXSR      OUTSFL
016400970526     C*
016500970526     C* GESTIONE TASTI FUNZIONALI
016600970526     C                   EXSR      FUNCON
016700970526     C*
016800970526E   2C                   ENDDO
016900970527     C*
017000970527     C* CI SONO ERRORI IN ENTRATA
017100970527X   1C                   ELSE
017200070124     C                   MOVEL     '1'           d09OERR
017300070124     C                   MOVEL     DSMSMS        d09OMSG
017400970526E   1C                   ENDIF
017500070129     C*
017600070129     C* CHIUDO IL FILE VIDEO
017700070129     C                   CLOSE     FIDG09D
017800070124E   1C                   ENDIF
017900070124     C*
018000070124     C* RITORNO PARAMETRI
018100070124     C                   MOVEL(P)  fidg09ds      KPJBU
018200970526     C* FINE PGM
018300970526     C                   SETON                                        LR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F03GES        BEGSR
018800970526     C*
018900970526     C* IMPOSTA "PREMUTO F03"
019000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019100070124     C                   MOVEL     '1'           d09OF03                        *DI PROCEDURA
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019600970526     C*------------------------------------------------------------------------*
019700970526     C     F05GES        BEGSR
019800970526     C*
019900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020000970526     C*
020100970526     C                   ENDSR
020200070202     C*------------------------------------------------------------------------*
020300070202     C* F08GES - GESTIONE F08 -> STAMPA
020400070202     C*------------------------------------------------------------------------*
020500070202     C     F08GES        BEGSR
020600070202     C*
020700070202     C* Apro il prtf
020800070202     C                   open      fidg09p
020900070202     C*
021000070202     C* Stampo testata
021100070202     C                   write     dg09pte
021200070202     C                   write     dg09pt
021300070202     C                   write     dg09pri
021400070202     C                   setoff                                       05
021500070202     C*
021600070202     C                   SETON                                        60
021700070202     C                   EXSR      POSSFL                                       *RICARICA SFILE
021800070202     C                   SETOFF                                       60
021900070202     C*
022000070202     C* Stampo chiusura prospetto
022100070202     C                   write     dg09pri
022200070202     C                   write     dg09pbk
022300070202     C                   write     dg09pbk
022400070202     C                   write     dg09pfi
022500070202     C*
022600070202     C* Quindi chiudo il prtf
022700070202     C                   close     fidg09p
022800070202     C*
022900070202     C                   ENDSR
023000070119     C*------------------------------------------------------------------------*
023100070119     C* F10GES - GESTIONE F10 -> ORDINAMENTO X CODICE GIRO
023200070119     C*------------------------------------------------------------------------*
023300070119     C     F10GES        BEGSR
023400070119     C*
023500070123     C                   EVAL      WTIPRI = 'CGI'
023600070124     C                   Z-ADD     1             swmodi
023700070119     C                   EXSR      POSSFL                                       *RICARICA SFILE
023800070119     C*
023900070119     C                   ENDSR
024000070119     C*------------------------------------------------------------------------*
024100070119     C* F11GES - GESTIONE F11 -> ORDINAMENTO X DESCRIZIONE GIRO
024200070119     C*------------------------------------------------------------------------*
024300070119     C     F11GES        BEGSR
024400070119     C*
024500070119     C                   EVAL      WTIPRI = 'DES'
024600070124     C                   Z-ADD     1             swmodi
024700070119     C                   EXSR      POSSFL                                       *RICARICA SFILE
024800070119     C*
024900070119     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
025200970526     C*------------------------------------------------------------------------*
025300970526     C     F06GES        BEGSR
025400970526     C*
025500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025600070124     C                   Z-ADD     *ZEROS        d09idde
025700070124     C                   Z-ADD     *ZEROS        d09ifgs
025800070124     C                   MOVEL     *BLANKS       d09icgi
025900970602     C                   MOVEL     DEPOP0        WOP0
026000970529     C                   MOVEL     'F06'         WOP1
026100070119     C                   MOVEL     'FIDG10R'     WPGM              8
026200970526     C                   EXSR      LANPGM                                       *LANCIA PGM
026300070423     C                   Z-ADD     1             swmodi
026400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026500970526E   1C                   ENDIF
026600970526     C*
026700970526     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900970526     C* F12GES - GESTIONE F12 -> RITORNO
027000970526     C*------------------------------------------------------------------------*
027100970526     C     F12GES        BEGSR
027200970526     C*
027300970526     C* IMPOSTA "PREMUTO F12"
027400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
027500070124     C                   MOVEL     '1'           d09OF12                         *DI PROCEDURA
027600970526     C*
027700970526     C                   ENDSR
027800970528     C*------------------------------------------------------------------------*
027900070119     C* F21GES - GESTIONE F21 -> STAMPA ELENCO SECONDO SELEZIONI A VIDEO
028000970528     C*------------------------------------------------------------------------*
028100970528     C     F21GES        BEGSR
028200970612     C*
028300070124     C                   Z-ADD     *ZEROS        d09idde
028400070124     C                   Z-ADD     *ZEROS        d09ifgs
028500070124     C                   MOVEL     *BLANKS       d09icgi
028600970612     C                   MOVEL     DEPOP0        WOP0
028700970612     C                   MOVE      '03'          WOP0
028800970612     C                   MOVEL     'F21'         WOP1
028900070119     C                   MOVEL     'FIDG11R'     WPGM              8
029000070119     C                   EXSR      LANPGM
029100970528     C*
029200970528     C                   ENDSR
029300970528     C*------------------------------------------------------------------------*
029400970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
029500970528     C*------------------------------------------------------------------------*
029600970528     C     F22GES        BEGSR
029700970528     C*
029800970528     C                   Z-ADD     80            LENGH            15 5
029900970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
030000970528     C                   PARM                    CMD(1)
030100970528     C                   PARM                    LENGH
030200970528     C*
030300970528     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
030600970526     C*------------------------------------------------------------------------*
030700970526     C     F23GES        BEGSR
030800970526     C*
030900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
031400970526     C*------------------------------------------------------------------------*
031500970526     C     F24GES        BEGSR
031600970526     C*
031700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031800970526     C*
031900970526     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
032200970526     C*------------------------------------------------------------------------*
032300970526     C     RUPGES        BEGSR
032400970526     C*
032500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
032600970526     C*
032700970526     C                   ENDSR
032800070202     C*------------------------------------------------------------------------*
032900070202     C* STAMPA - Stampa giro corrente
033000070202     C*------------------------------------------------------------------------*
033100070202     C     STAMPA        BEGSR
033200070202     C*
033300070202     C* Valorizzo i dati in stampa
033400070202     C                   eval      stpatb   = grgatb
033500070202     C                   eval      stpfgs   = grgfgs
033600070202     C                   eval      stpgiro  = grgcgi
033700070202     C                   eval      stpgirod = grgdes
033800070202     C                   eval      stpain   = grgain
033900070202     C                   eval      stpdde   = viddde
034000070202     C                   eval      stpdscad = viddscad
034100070202     C                   if        grgact = 'N'
034200070202     C                   eval      stpact   = 'No'
034300070202     C                   else
034400070202     C                   eval      stpact   = 'Sì'
034500070202     C                   endif
034600070320     C                   eval      DGRGFLO = grgflo
034700070320     C                   if        §grgfit = 'S'
034800070320     C                   eval      stpfit   = 'Sì'
034900070320     C                   else
035000070320     C                   eval      stpfit   = 'No'
035100070320     C                   endif
035200070320     C                   if        §grgdst = 'N'
035300070320     C                   eval      stpdst   = 'No'
035400070320     C                   else
035500070320     C                   eval      stpdst   = 'Sì'
035600070320     C                   endif
035700070320     C                   if        §grgedp = 'S'
035800070320     C                   eval      stpedp   = 'Sì'
035900070320     C                   else
036000070320     C                   eval      stpedp   = 'No'
036100070320     C                   endif
036200110913     c                   clear                   stpztl
036300110913     c                   if        §grgcsto='S'
036400110913     c                   eval      stpztl='ZTL '
036500110913     c                   endif
036600110913     c
036700070202     C*
036800070202     C* Gestisco salto pagina
036900070202     C                   if        *in05 = *on
037000070202     C                   write     dg09pri
037100070202     C                   write     dg09pte
037200070202     C                   write     dg09pt
037300070202     C                   write     dg09pri
037400070202     C                   setoff                                       05
037500070202     C                   endif
037600070202     C*
037700070202     C* Quindi stampo giro corrente
037800070202     C                   write     dg09pd
037900070202     C*
038000070202     C                   ENDSR
038100970526     C*------------------------------------------------------------------------*
038200970526     C* ENTGES - GESTIONE ENTER -> INVIO
038300970526     C*------------------------------------------------------------------------*
038400970526     C     ENTGES        BEGSR
038500070124     C*
038600070124     C* CONTROLLO CHE IL P.O. INDICATO IN SELEZIONE SIA EFFETTIVAMENTE IN GESTIONE
038700070124     C* ALL'UTENTE CORRENTE
038800070124     C                   MOVEL     'S'           wAutPO            1
038900070601     C                   IF        %subst(KNMUS:1:3) <> 'EDP' AND
039000070605     C                             VI1FGS  = *ZEROS  and
039100070605      * se utente isp99 deve comportarsi come edp per i controlli del P.O. selezione
039200070605     C                             %subst(KNMUS:1:5) <> 'ISP99'
039300070601     C                   MOVEL     'N'           wAutPO
039400070601     C                   ENDIF
039500070124     C                   IF        DEPOP0 <> '001'  AND
039600070124     C                             DEPOP0 <> 'P01'  AND
039700070605     C                             %subst(KNMUS:1:3) <> 'EDP'  and
039800070605      * se utente isp99 deve comportarsi come edp per i controlli del P.O. selezione
039900070605     C                             %subst(KNMUS:1:5) <> 'ISP99'
040000070124     C                   CLEAR                   FNLV50DS
040100070124     C                   EVAL      D50TLA = 'L'
040200070124     C                   EVAL      D50PRU = KNMUS
040300070124     C                   EVAL      D50FGS = VI1FGS
040400070124     C                   CALL      'FNLV50R'
040500070124     C                   PARM                    FNLV50DS
040600070124     C                   IF        D50ERR <> *BLANKS
040700070124     C                   MOVEL     'N'           wAutPO
040800070124     C                   MOVEL     '1'           ERRL00
040900070124     C                   MOVEL     D50MSG        DSMSMS
041000070124     C                   ENDIF
041100070124     C                   ENDIF
041200070905      * se richiesto gestione delle filiali anche della £6 solo in '001' 'P01' verifico nella
041300070905      * schiera delle filiali
041400070905     C                   IF        (DEPOP0 = '001'  or
041500070905     C                             DEPOP0 = 'P01' ) AND
041600070905     C                             DEPf£6 = 'S'
041700070905      * verifico in schiera
041800070905     c     Vi1fgs        lookup    fil                                    30
041900070906     c                   If        not %found  or vi1fgs = 0
042000070905     C                   MOVEL     'N'           wAutPO
042100070905     C                   MOVEL     '1'           ERRL00
042200070905     C                   MOVEL     msg(8)        DSMSMS
042300070905     c                   endif
042400070905     c                   endif
042500070124     C*
042600070124     C                   IF        wAutPO = 'S'
042700000703     C*
042800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
042900970526     C                   EXSR      NEWPOS
043000970526     C*
043100970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
043200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043300970526IF  2C     SWPOSI        IFNE      0
043400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
043500970526X   2C                   ELSE
043600970526IF  3C     SFLULT        IFNE      0
043700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
043800970526     C*
043900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
044000970526IF  4C     SWMODI        IFNE      0
044100970526     C                   EXSR      POSSFL
044200970526E   4C                   ENDIF
044300970526E   3C                   ENDIF
044400970526E   2C                   ENDIF
044500970526E   1C                   ENDIF
044600970526     C*
044700070124     C                   ENDIF
044800070124     C*
044900970526     C                   ENDSR
045000000703     C*------------------------------------------------------------------------*
045100970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
045200970526     C*------------------------------------------------------------------------*
045300970526     C     O01GES        BEGSR
045400970526     C*
045500970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
045600070124     C                   Z-ADD     VIDFGS        d09ofgs
045700070124     C                   Z-ADD     VIHDDE        d09odde
045800070124     C                   Z-ADD     VIHDSCAD      d09odsc
045900070124     C                   MOVEL     VIDCGI        d09ocgi
046000070124     C                   MOVEL     VIDDES        d09odes
046100070124     C                   MOVEL     VIDAIN        d09oain
046200070124     C                   MOVEL     VIHATB        d09oatb
046300070124     C                   MOVEL     VIHACT        d09oact
046400070316     C                   MOVEL     VIHFLO        d09oflo
046500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046600970526     C*
046700970526     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
047000970526     C*------------------------------------------------------------------------*
047100970526     C     O02GES        BEGSR
047200970526     C*
047300970527     C* MODIFICA
047400970526IF  1C     ERRL00        IFEQ      '0'
047500070124     C                   Z-ADD     VIHDDE        d09idde
047600070124     C                   Z-ADD     VIDFGS        d09ifgs
047700070124     C                   MOVEL     VIDCGI        d09icgi
047800970602     C                   MOVEL     DEPOP0        WOP0
047900970526     C                   MOVEL     'O02'         WOP1
048000070119     C                   MOVEL     'FIDG10R'     WPGM
048100970526     C                   EXSR      LANPGM
048200070123     C                   Z-ADD     1             swmodi
048300970526E   1C                   ENDIF
048400970526     C*
048500970526     C                   ENDSR
048600991014     C*------------------------------------------------------------------------*
048700070119     C* O03GES - GESTIONE OPZIONE 3=COPIA
048800991014     C*------------------------------------------------------------------------*
048900991014     C     O03GES        BEGSR
049000991014     C*
049100070119     C* COPIA
049200991014IF  1C     ERRL00        IFEQ      '0'
049300070124     C                   Z-ADD     VIHDDE        d09idde
049400070124     C                   Z-ADD     VIDFGS        d09ifgs
049500070124     C                   MOVEL     VIDCGI        d09icgi
049600991014     C                   MOVEL     DEPOP0        WOP0
049700991014     C                   MOVEL     'O03'         WOP1
049800070119     C                   MOVEL     'FIDG10R'     WPGM
049900991014     C                   EXSR      LANPGM
050000070123     C                   Z-ADD     1             swmodi
050100991014E   1C                   ENDIF
050200991014     C*
050300991014     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
050600970526     C*------------------------------------------------------------------------*
050700070119     C     O04GES        BEGSR
050800070123     C*
050900070123     C* ANNULLAMENTO POSSIBILE SOLO SE RECORD NN GIA' ANNULLATO
051000070123     C                   IF        VIHATB <> *blanks
051100070123     C                   MOVEL     '1'           ERRL00
051200070123     C                   MOVEL     MSG(4)        DSMSMS
051300070123     C                   ELSE
051400070119     C*
051500970527     C* ANNULLAMENTO
051600070119IF  1C     ERRL00        IFEQ      '0'
051700070124     C                   Z-ADD     VIHDDE        d09idde
051800070124     C                   Z-ADD     VIDFGS        d09ifgs
051900070124     C                   MOVEL     VIDCGI        d09icgi
052000070119     C                   MOVEL     DEPOP0        WOP0
052100070119     C                   MOVEL     'O04'         WOP1
052200070119     C                   MOVEL     'FIDG10R'     WPGM
052300070119     C                   EXSR      LANPGM
052400070123     C                   Z-ADD     1             swmodi
052500070123     C                   SETON                                        71
052600070119E   1C                   ENDIF
052700070123     C                   ENDIF
052800970526     C*
052900070122     C                   ENDSR
053000970526     C*------------------------------------------------------------------------*
053100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
053200970526     C*------------------------------------------------------------------------*
053300970526     C     O05GES        BEGSR
053400970526     C*
053500070124     C                   Z-ADD     VIHDDE        d09idde
053600070124     C                   Z-ADD     VIDFGS        d09ifgs
053700070124     C                   MOVEL     VIDCGI        d09icgi
053800970602     C                   MOVEL     DEPOP0        WOP0
053900970526     C                   MOVEL     'O05'         WOP1
054000070119     C                   MOVEL     'FIDG10R'     WPGM
054100970526     C                   EXSR      LANPGM
054200070423     C                   Z-ADD     1             swmodi
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
054700970526     C*------------------------------------------------------------------------*
054800070123     C     O07GES        BEGSR
054900070123     C*
055000070123     C* RIPRISTINO POSSIBILE SOLO SE RECORD ANNULLATO
055100070123     C                   IF        VIHATB = *blanks
055200070123     C                   MOVEL     '1'           ERRL00
055300070123     C                   MOVEL     MSG(5)        DSMSMS
055400070123     C                   ELSE
055500970526     C*
055600970526     C* RIPRISITINO
055700070119IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
055800070124     C                   Z-ADD     VIHDDE        d09idde
055900070124     C                   Z-ADD     VIDFGS        d09ifgs
056000070124     C                   MOVEL     VIDCGI        d09icgi
056100070119     C                   MOVEL     DEPOP0        WOP0
056200070119     C                   MOVEL     'O07'         WOP1
056300070119     C                   MOVEL     'FIDG10R'     WPGM
056400070119     C                   EXSR      LANPGM
056500070123     C                   Z-ADD     1             SWMODI
056600070123     C                   SETOFF                                       71
056700070122E   1C                   ENDIF
056800070123     C                   ENDIF
056900970526     C*
057000970526     C                   ENDSR
057100970526     C*------------------------------------------------------------------------*
057200970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
057300970526     C*------------------------------------------------------------------------*
057400970526     C     NEWPOS        BEGSR
057500970526     C*
057600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
057700970526     C*         1= RICHIESTO POSIZIONAMENTO
057800970526     C                   Z-ADD     0             SWPOSI            7 0
057900970526     C*
058000970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
058100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
058200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
058300970526     C                   Z-ADD     0             SWMODI            1 0
058400970526     C*
058500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
058600070119IF  1C     VI1FGS        IFNE      depFGS
058700070123     C     VI1ATB        ORNE      depATB
058800070119     C     VI1AIN        ORNE      depAIN
058900070119     C     VI1DES        ORNE      depDES
059000070119     C     VI1DAT        ORNE      depDAT
059100070123     C     VI1CGI        ORNE      depCGI
059200110624     C     VI1csto       ORNE      depcsto
059300110624     C     VI1fit        ORNE      depfit
059400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
059500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
059600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
059700070119     C                   Z-ADD     VI1FGS        depFGS                         *DEPOSITI
059800070119     C                   MOVEL     VI1AIN        depAIN                         *DEPOSITI
059900070123     C                   MOVEL     VI1ATB        depATB                         *DEPOSITI
060000070119     C                   MOVEL     VI1DES        depDES                         *DEPOSITI
060100070119     C                   MOVEL     VI1DAT        depDAT                         *DEPOSITI
060200070123     C                   MOVEL     VI1CGI        depCGI                         *DEPOSITI
060300110624     C                   MOVEL     VI1csto       depCsto
060400110624     C                   MOVEL     VI1fit        depfit
060500970526E   2C                   ENDIF
060600970526E   1C                   ENDIF
060700970526     C*
060800970526     C                   ENDSR
060900970526     C*------------------------------------------------------------------------*
061000970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
061100970526     C*------------------------------------------------------------------------*
061200970528     C     SETFIL        BEGSR
061300970526     C*
061400970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
061500070123IF  1C     WTIPRI        IFEQ      'CGI'                                        *PER CODICE GIRO
061600070123     C     *loval        SETLL     figrg01l
061700970528E   1C                   ENDIF
061800070119     C*
061900070119IF  1C     WTIPRI        IFEQ      'DES'                                        *PER DESCRIZ. GIRO
062000070123     C     *loval        SETLL     figrg02l
062100070119E   1C                   ENDIF
062200071025     C*
062300071025IF  1C     WTIPRI        IFEQ      'NOV'                                        *PER FIL/GIRO
062400071025     C                   IF        DEPOP0='001'
062500071025     C     KEY_GRG03_P   SETLL     figrg03l
062600071025     C                   ELSE
062700071025     C     *loval        SETLL     figrg03l
062800071025     C                   ENDIF
062900071025E   1C                   ENDIF
063000980107     C*
063100970526     C* LETTURA RECORD
063200970526IF  1C     *IN99         IFEQ      *OFF
063300970526     C                   EXSR      LETFIL
063400970526E   1C                   ENDIF
063500970526     C*
063600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063700970526IF  1C     *IN99         IFEQ      *ON
063800970526     C                   Z-ADD     1             SFLEOF
063900970526E   1C                   ENDIF
064000970526     C*
064100970526     C                   ENDSR
064200970526     C*------------------------------------------------------------------------*
064300970526     C* LETFIL - LETTURA RECORD
064400970526     C*------------------------------------------------------------------------*
064500970526     C     LETFIL        BEGSR
064600970526     C*
064700970526     C* LEGGE FINO A:
064800970526     C*     - FINE FILE
064900970526     C*     - TROVATO RECORD VALIDO
065000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
065100010306     C     *IN99         DOWEQ     *OFF
065200970526     C     WRECOK        ANDEQ     'N'
065300970526     C*
065400970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
065500070123     C     WTIPRI        IFEQ      'CGI'                                        *PER CODICE GIRO
065600070119     C                   READ      figrg01l                               99
065700010306     C                   ENDIF
065800970528     C*
065900070119     C     WTIPRI        IFEQ      'DES'                                        *PER DESCRIZ. GIRO
066000070119     C                   READ      figrg02l                               99
066100070119     C                   ENDIF
066200071025     C*
066300071025     C     WTIPRI        IFEQ      'NOV'                                        *PER FIL/GIRO
066400071025     C                   READ      figrg03l                               99
066500071025     C                   ENDIF
066600070119     C*
066700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
066800010306     C     *IN99         IFEQ      *ON                                          *FINE FILE
066900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
067000010306     C                   ELSE
067100071025     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
067200071025     C*
067300071025     C* Se passata chiave in input => esco dalla lettura
067400071025     C                   if        DEPOP0 = '001'
067500071025     C                   if        grgFGS <> D09IFGS AND
067600071025     C                             grgCGI <> D09ICGI
067700071025     C                   movel     '1'           F03L00
067800071025     C                   movel     '1'           SFLEOF
067900071025     C                   seton                                        99
068000071025     C                   endif
068100071025     C                   endif
068200071025     C*
068300010306     C                   ENDIF
068400010306     C                   ENDDO
068500970526     C*
068600970526     C                   ENDSR
068700970526     C*------------------------------------------------------------------------*
068800970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
068900970526     C*------------------------------------------------------------------------*
069000970526     C     CHKREC        BEGSR
069100970526     C*
069200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
069300070622     C                   MOVEL     GRGFLO        DGRGFLO
069400010117     C*
069500070119     C* FILTRO X P.O. GESTIONE
069600010427     C                   if        WRECOK = 'S'
069700070119     C     VI1FGS        ifeq      *zeros
069800010117     C                   else
069900070119     C                   if        grgfgs <> VI1FGS
070000010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
070100010117     C                   endif
070200070119     C                   endif
070300010427     C                   endif
070400070119     C*
070500070119     C* FILTRO X AMBITO
070600070119     C                   if        WRECOK = 'S'
070700070119     C     VI1AIN        ifeq      *blanks
070800070119     C                   else
070900070119     C                   if        grgain <> VI1AIN
071000070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
071100070119     C                   endif
071200070119     C                   endif
071300070119     C                   endif
071400070119     C*
071500070119     C* FILTRO X ALLA DATA
071600070119     C                   if        WRECOK = 'S'
071700070123     C     VI1DAT        ifeq      *zeros
071800070123     C                   else
071900070123     C*
072000070123     C                   z-add     VI1DAT        G08DAT
072100070123     C                   z-add     *ZEROS        G08INV
072200070123     C                   movel     '0'           G08ERR
072300070123     C                   call      'XSRDA8'
072400070123     C                   parm                    WLBDA8
072500070123     C                   z-add     G08INV        wvi1dat
072600070123     C*
072700070119     C                   if        wVI1DAT >= grgdde AND
072800070124     C                             wVI1DAT <= grgdscad
072900070119     C                   else
073000070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
073100070119     C                   endif
073200070119     C                   endif
073300070119     C                   endif
073400010427     C*
073500070119     C* FILTRA X DESCRIZIONE GIRO (IN "LIKE")
073600010427     C                   if        WRECOK = 'S'
073700070119     C     VI1DES        ifeq      *blanks
073800010709     C                   else
073900010709     C                   z-add     *zeros        pos               2 0
074000070119     C                   eval      pos = %scan(%trim(VI1DES):grgdes)
074100010709     C                   if        pos = *zeros
074200010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
074300010427     C                   endif
074400010427     C                   endif
074500010427     C                   endif
074600070119     C*
074700070119     C* FILTRO X FLAG ANNULLAMENTO
074800070119     C                   if        WRECOK = 'S'
074900070123     C                   if        VI1ATB <> *blanks
075000070122     C                   if        VI1ATB <> grgatb
075100070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
075200070119     C                   endif
075300070123     C                   endif
075400070119     C                   endif
075500070122     C*
075600070122     C* FILTRO X CODICE GIRO
075700070122     C                   if        WRECOK = 'S'
075800070123     C     VI1CGI        ifeq      *blanks
075900070122     C                   else
076000070123     C                   if        grgcgi <> VI1CGI
076100070122     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
076200070122     C                   endif
076300070122     C                   endif
076400070122     C                   endif
076500070411     C*
076600070411     C* FILTRO X DATA DECORRENZA (SOLO DA *PGM CHIAMANTE)
076700070411     C                   if        WRECOK = 'S'
076800070411     C                   if        d09idde <> *zeros
076900070411     C                   if        d09idde <> grgdde
077000070411     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
077100070411     C                   endif
077200070411     C                   endif
077300070411     C                   endif
077400070622     C*
077500070622     C* FILTRO X tipo utilizzo giro
077600070622     C                   if        WRECOK = 'S'
077700070703     C                   if        D09ITUG = *BLANK                             NESSUNA SELEZIONE
077800070622     C                   else
077900070703      * se tipo utilizzo del giro non è uguale alla selezione fatta oppure
078000070703      * non è tipo utilizzo E=entrambi il record non è valido
078100070703     C                   if        §grgTUG <> D09itug and §grgTUG <>'E'
078200070622     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
078300070622     C                   endif
078400070622     C                   endif
078500070622     C                   endif
078600110624     C* FILTRO X fittizio
078700110624     C                   if        WRECOK = 'S'
078800110624     C                   if        (§grgfit = ' ' and vi1fit='S') or
078900110624     C                             (§grgfit = 'S' and vi1fit='N')
079000110624     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079100110624     C                   endif
079200110624     C                   endif
079300110624     c* giri centro storico
079400110624     C                   if        WRECOK = 'S'
079500110624     C                   if        §grgcsto= ' ' and vi1csto='S'
079600110624     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079700110624     C                   endif
079800110624     C                   endif
079900970526     C*
080000970526     C                   ENDSR
080100970526     C*------------------------------------------------------------------------*
080200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
080300970526     C*------------------------------------------------------------------------*
080400970526     C     IRESFL        BEGSR
080500070119     C*
080600070119     C                   setoff                                       71
080700970526     C*
080800070119     C                   movel     *BLANKS       OPZKEY
080900070123     C                   movel     grgcgi        VIDCGI
081000070119     C                   movel     grgdes        VIDDES
081100070123     C                   movel     grgain        VIDAIN
081200070119     C                   movel     grgatb        VIHATB
081300070119     C                   movel     grgact        VIHACT
081400070316     C                   movel     grgflo        VIHFLO
081500070119     C                   z-add     grgdde        VIHDDE
081600070124     C                   z-add     grgdscad      VIHDSCAD
081700070119     C                   z-add     grgfgs        VIDFGS
081800070119     C*
081900070119     C* Se giro annullato metto in reverse il codice giro stesso
082000070119     C                   if        grgatb <> *blanks
082100070119     C                   seton                                        71
082200070119     C                   endif
082300110624     c
082400110624     c                   clear                   vidtip
082500110624     c                   movel     grgflo        dgrgflo
082600110624     c
082700110624     c* Visulizzo se giro centro storico o fittizio
082800110624     c                   select
082900110624     c                   when      §grgcsto='S' and §grgfit='S'
083000110913     c                   eval      vidtip='ZT/F'
083100110624     c
083200110624     c                   when      §grgcsto='S'
083300110913     c                   eval      vidtip='ZTL '
083400110624     c
083500110624     c                   when      §grgfit='S'
083600110913     c                   eval      vidtip='Fit '
083700110624     c                   endsl
083800110624     c
083900070119     C* Inverto a video data decorrenza
084000070119     C                   Z-ADD     grgdde        G08INV
084100991025     C                   Z-ADD     *ZEROS        G08DAT
084200991025     C                   MOVEL     '3'           G08ERR
084300991025     C                   CALL      'XSRDA8'
084400991025     C                   PARM                    WLBDA8
084500070119     C                   Z-ADD     G08DAT        VIDDDE
084600070119     C*
084700070119     C* Inverto a video data scadenza
084800070124     C                   Z-ADD     grgdscad      G08INV
084900991025     C                   Z-ADD     *ZEROS        G08DAT
085000991025     C                   MOVEL     '3'           G08ERR
085100991025     C                   CALL      'XSRDA8'
085200991025     C                   PARM                    WLBDA8
085300070124     C                   Z-ADD     G08DAT        VIDDSCAD
085400070202     C*
085500070202     C* Gestisco se richiesta la stampa dei giri (secondo selezione corrente)
085600070202     C   60              exsr      stampa
085700011129     C*
085800970526     C                   ENDSR
085900970526     C*------------------------------------------------------------------------*
086000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
086100970526     C*------------------------------------------------------------------------*
086200070119     C     CHKPAR        BEGSR
086300970528     C*
086400000801     C*
086500970526     C                   ENDSR
086600970526     C*------------------------------------------------------------------------*
086700970526     C* LANPGM - LANCIA UN PROGRAMMA
086800970526     C*------------------------------------------------------------------------*
086900970526     C     LANPGM        BEGSR
087000970526     C*
087100970526     C* IMPOSTA LA DS DI PROCEDURA
087200070124     C                   MOVEL     WOP0          d09IOP0                        *LIVELLO PROCEDURA
087300070124     C                   MOVEL     WOP1          d09IOP1                        *LIVELLO PGM
087400070124     C                   MOVEL     '0'           d09OF03                        *PREMUTO F03=FINE
087500070124     C                   MOVEL     '0'           d09OF12                        *PREMUTO F12=RITORNO
087600070124     C                   MOVEL     '0'           d09OERR                        *'1'=ERRORE
087700070124     C                   MOVEL     *BLANKS       d09OMSG                        *MESSAGGIO DI RITORN
087800070123     C                   MOVEL(P)  fidg09ds      KPJBU
087900000705     C*
088000970526     C                   CALL      WPGM
088100970612     C                   PARM                    KPJBA
088200070123     C                   MOVEL(P)  KPJBU         fidg09ds
088300970526     C*
088400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
088500070124IF  1C     d09OF03       IFEQ      '1'                                          *PREMUTO F03
088600070122     C                   MOVEL     '1'           ERRL00                         *ERRORE
088700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
088800970526X   1C                   ELSE                                                   *NON PREMUTO F03
088900070124     C                   MOVEL     d09OMSG       DSMSMS                        *MESSAGGIO DI RITORN
089000970526E   1C                   ENDIF
089100970526     C*
089200970526     C                   ENDSR
089300970526     C*------------------------------------------------------------------------*
089400070119     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
089500970526     C*------------------------------------------------------------------------*
089600970526     C     CHKFOP        BEGSR
089700070122     C*
089800070122     C                   MOVEL     FUD(10)       FUDRIG                         *ORDINAMENTO
089900070122     C                   MOVEL     'S'           FUDATT
090000070122     C                   MOVEL     FUDRIG        FUD(10)
090100070122     C                   MOVEL     FUD(11)       FUDRIG                         *ORDINAMENTO
090200070122     C                   MOVEL     'S'           FUDATT
090300070122     C                   MOVEL     FUDRIG        FUD(11)
090400970626     C*
090500970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
090600970626IF  1C     DEPOP0        IFEQ      'P01'
090700970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
090800970626     C                   MOVEL     'S'           OPDATT
090900970626     C                   MOVEL     OPDRIG        OPD(1)
091000970626X   1C                   ELSE
091100970626     C* OPZIONI
091200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
091300970626     C                   MOVEL     'S'           OPDATT
091400970626     C                   MOVEL     OPDRIG        OPD(5)
091500970626     C* FUNZIONI
091600970626E   1C                   ENDIF
091700970626     C*
091800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
091900070122IF  1C     DEPOP0        IFEQ      'P03'
092000070122     C     DEPOP0        OREQ      'P04'
092100970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
092200970626     C                   MOVEL     'S'           OPDATT
092300970626     C                   MOVEL     OPDRIG        OPD(2)
092400070122     C***                MOVEL     OPD(3)        OPDRIG                         *COPIA
092500070122     C***                MOVEL     'S'           OPDATT
092600070122     C***                MOVEL     OPDRIG        OPD(3)
092700070601     C*
092800070601     C* ANNULLAMENTO CONSENTITO SOLO A PERSONALE EDP*
092900070601     C                   IF        %subst(KNMUS:1:3)  = 'EDP'
093000070119     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
093100070119     C                   MOVEL     'S'           OPDATT
093200070119     C                   MOVEL     OPDRIG        OPD(4)
093300070601     C                   ENDIF
093400070601     C*
093500070119     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
093600070119     C                   MOVEL     'S'           OPDATT
093700070119     C                   MOVEL     OPDRIG        OPD(7)
093800970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
093900970626     C                   MOVEL     'S'           FUDATT
094000970626     C                   MOVEL     FUDRIG        FUD(6)
094100970626E   1C                   ENDIF
094200070119     C*
094300070124     C* SE IN SCELTA INOLTRE DISABILITO ALCUNE SELEZIONI A VIDEO
094400070904     C                   SETON                                        7092
094500070124IF  1C     DEPOP0        IFEQ      'P01'
094600070904     C                   SETOFF                                       70
094700070124     C                   SETOFF                                       93
094800070904      * se non devo impostare la filiale di gestione
094900070904     c                   If        depf£6 = ' '
095000070904     c                   setoff                                       92
095100070904     c                   endif
095200070904
095300070119E   1C                   ENDIF
095400970526     C*
095500970526     C* GESTIONE OPZIONI A VIDEO
095600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
095700970526     C                   EXSR      OPZGES
095800970526     C*
095900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
096000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
096100970526     C                   EXSR      FUNGES
096200970526     C*
096300970526     C                   ENDSR
096400020916     C*--------------------------------------------------------------------------------------------*
096500070119     C* REPERISCE I DATI UTENTE
096600020916     C*--------------------------------------------------------------------------------------------*
096700020916     C     REPDATIUTE    BEGSR
096800020916     C*
096900020916     C* INIZIALIZZA VARIABILI DI WRK
097000020916     C                   CLEAR                   TIBS34DS
097100020916     C                   CLEAR                   AZUTEDS
097200020916     C                   CLEAR                   DDATIUTE
097300020916     C*
097400020916     C     *DTAARA       DEFINE    §azute        azuteds
097500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
097600020916     C                   IN(E)     *DTAARA
097700020916if  1C                   IF        %Error
097800020916     c                   EVAL      I34Tla = 'L'
097900020916     C                   CALL      'TIBS34R'
098000020916     C                   PARM                    Tibs34Ds
098100020916     C                   IN        *DTAARA
098200020916e   1C                   ENDIF
098300020916     C*
098400020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
098500020916     C                   MOVEL(P)  RSUT          DSFIRS           20
098600020916     C*
098700020916     C                   ENDSR
098800970526     C*------------------------------------------------------------------------*
098900970526     C* *INZSR - OPERAZIONI INIZIALI
099000970526     C*------------------------------------------------------------------------*
099100970526     C     *INZSR        BEGSR
099200970526     C*---
099300970526     C* RICEVIMENTO PARAMETRI
099400970526     C*---
099500970526     C     *ENTRY        PLIST
099600970612     C                   PARM                    KPJBA
099700071025     C*
099800071025     C* CHIAVI DI ACCESSO
099900071025     C*
100000071025     C* FIGRG03L - PARZIALE
100100071025     C     KEY_GRG03_P   KLIST
100200071025     C                   KFLD                    D09IFGS
100300071025     C                   KFLD                    D09ICGI
100400071025     C*
100500070905     C                   TIME                    W0140            14 0
100600070905      * UDATE IN GGMMAAAA
100700070905     C                   MOVE      W0140         oggi_ggmmaa       8 0
100800070905      * UDATE IN AAAAMMGG
100900070905     C                   Z-ADD     oggi_ggmmaa   G08DAT
101000070905     C                   MOVEL     *BLANK        G08ERR
101100070905     C                   CALL      'XSRDA8'
101200070905     C                   PARM                    WLBDA8
101300070905     C                   Z-ADD     G08INV        oggi_aammgg       8 0
101400070306     C*
101500070306     C* REPERIMENTO DATI UTENTE
101600070306     C                   EXSR      REPDATIUTE
101700070122     C*
101800070125     C                   IF        %subst(KNMUS:1:3) = 'EDP'
101900070125     C                   SETON                                        93
102000070125     C                   ELSE
102100070124     C                   SETOFF                                       93
102200070125     C                   ENDIF
102300070124     C*
102400070905
102500070905     c                   clear                   fil
102600070905
102700070122     C                   IF        KPJBU <> *blanks
102800070119     C                   MOVEL     KPJBU         fidg09ds
102900070122     C*
103000070122     C* DEPOSITA LE OPZIONI DI ENTRATA
103100070124     C                   MOVEL     d09IOP0       DEPOP0                         *LIVELLO PROCEDURA
103200070124     C                   MOVEL     d09IOP1       DEPOP1                         *LIVELLO PROGRAMMA
103300070904     C                   MOVEL     d09IF£6       DEPf£6                         *Richiesta fil gest
103400070122     C*
103500070122     C* IMPOSTA I PARAMETRI DI INGRESSO
103600070124     C                   MOVE(P)   d09ifgs       VI1FGS
103700070124     C                   MOVE(P)   d09ifgs       depFGS
103800070124     C                   Z-ADD     d09idat       wVI1DAT
103900070124     C                   MOVEL(P)  d09icgi       VI1CGI
104000070124     C                   MOVEL(P)  d09icgi       depCGI
104100070124     C                   MOVEL(P)  d09ides       VI1DES
104200070124     C                   MOVEL(P)  d09ides       depDES
104300070124     C                   MOVEL(P)  d09iain       VI1AIN
104400070124     C                   MOVEL(P)  d09iain       depAIN
104500070124     C                   MOVEL(P)  d09iatb       VI1ATB
104600070124     C                   MOVEL(P)  d09iatb       depATB
104700070905      * se flag richiesta filiale di gestione è diverso da blank recupero le filiali
104800070905     c                   If        depf£6 =  'S'
104900070905     c                   exsr      recfil
105000070905     c                   endif
105100070405     C*
105200070405     C* Se nn rischiesta emissione file video forzo i parametri d selezione anche nei
105300070405     C* campi del video
105400070405     C                   IF        DEPOP0 = '001'
105500070405     C                   MOVE(P)   d09ifgs       VI1FGS
105600070405     C                   MOVEL(P)  d09icgi       VI1CGI
105700070405     C                   MOVEL(P)  d09ides       VI1DES
105800070405     C                   MOVEL(P)  d09iain       VI1AIN
105900070405     C                   MOVEL(P)  d09iatb       VI1ATB
106000070405     C                   Z-ADD     d09idat       wVI1DAT
106100070405     C                   ENDIF
106200070122     C*
106300070122     C* Inverto la data "validi al"
106400070122     C                   Z-ADD     wVI1DAT       G08INV
106500070122     C                   Z-ADD     *ZEROS        G08DAT
106600070122     C                   MOVEL     '3'           G08ERR
106700070122     C                   CALL      'XSRDA8'
106800070122     C                   PARM                    WLBDA8
106900070122     C                   Z-ADD     G08DAT        VI1DAT
107000070124     C                   Z-ADD     G08DAT        depDAT
107100070122     C*
107200070122     C                   EXSR      CHKPAR
107300070122IF  1C     ERRL00        IFEQ      '1'
107400070124     C                   MOVEL     '1'           d09OERR
107500070124     C                   MOVEL     DSMSMS        d09OMSG
107600070122E   1C                   ENDIF
107700070122     C*
107800070122     C* SE ENTRATA DA MENU' GESTISCO IN BASE ALL'UTENTE
107900070122     C                   ELSE
108000070122     C                   CLEAR                   fidg09ds
108100070122     C                   IF        %subst(KNMUS:1:3) = 'EDP'
108200070122     C                   EVAL      DEPOP0 = 'P04'
108300070122     C                   ELSE
108400070306     C                   EVAL      VI1FGS = DUTPOU
108500070124     C                   EVAL      DEPOP0 = 'P03'
108600070122     C                   ENDIF
108700070122     C                   ENDIF
108800970526     C*
108900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
109000970526     C     XMSGPA        PLIST
109100970526     C                   PARM                    DSMSMS
109200970526     C                   PARM                    DSMSPG
109300970526     C                   PARM                    DSMSKE
109400970526     C                   PARM                    DSMSER
109500970526     C*
109600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
109700970526     C                   CALL      'XKEY'
109800970526     C                   PARM                    DSKEY
109900070119     C*
110000070119     C* DI DEFAULT IMPOSTO L?ORDINAMENTO X CODICE GIRO
110100070123     C                   MOVEL     'CGI'         WTIPRI            3
110200071025     C                   IF        DEPOP0 = '001'
110300071025     C                   MOVEL     'NOV'         WTIPRI
110400071025     C                   ENDIF
110500970526     C*---
110600970526     C* VARIABILI NON RIFERITE AL DATA BASE
110700970526     C*---
110800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
110900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
111000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
111100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
111200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
111300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
111400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
111500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
111600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
111700070124     C     *LIKE         DEFINE    d09IOP0       DEPOP0                         *DI DEPOSITO
111800070124     C     *LIKE         DEFINE    d09IOP1       DEPOP1
111900070904     C     *LIKE         DEFINE    d09IF£6       DEPF£6
112000070124     C     *LIKE         DEFINE    d09IOP0       WOP0                           *DI LAVORO
112100070124     C     *LIKE         DEFINE    d09IOP1       WOP1
112200970526     C*---
112300110624     C     *LIKE         DEFINE    §grgcsto      Depcsto
112400110624     C     *LIKE         DEFINE    §grgfit       Depfit
112500110624     C     *LIKE         DEFINE    d09iain       DepAIN
112600070124     C     *LIKE         DEFINE    d09iatb       DepATB
112700070124     C     *LIKE         DEFINE    d09ides       DepDES
112800070124     C     *LIKE         DEFINE    d09idat       DepDAT
112900070124     C     *LIKE         DEFINE    d09idat       wVI1DAT
113000070124     C     *LIKE         DEFINE    d09icgi       DepCGI
113100070124     C     *LIKE         DEFINE    d09ifgs       DepFGS
113200001020     C*---
113300970526     C* VARIABILI DI CONTROLLO
113400970526     C*---
113500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
113600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
113700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
113800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
113900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
114000070201     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
114100970526     C*
114200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
114300070119     C                   MOVEL     'FIDG09R'     DSMSPG
114400070119     C                   MOVEL     'FIDG09R'     MSGQUE
114500970526     C                   MOVEL     *BLANKS       MSGKEY
114600970526     C                   MOVEL     *BLANKS       DSMSMS
114700970526     C                   MOVEL     *BLANKS       DSMSKE
114800970526     C                   Z-ADD     *ZEROS        DSMSER
114900970526     C*
115000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
115100970526     C                   EXSR      CHKFOP
115200970526     C*
115300970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
115400970530IF  1C     DEPOP0        IFEQ      'P01'
115500070119     C                   MOVEL     DEC(1)        VIDING
115600970530E   1C                   ENDIF
115700970530IF  1C     DEPOP0        IFEQ      'P02'
115800070119     C                   MOVEL     DEC(2)        VIDING
115900970530E   1C                   ENDIF
116000070119IF  1C     DEPOP0        IFEQ      'P03'
116100070119     C                   MOVEL     DEC(3)        VIDING
116200070119E   1C                   ENDIF
116300070122IF  1C     DEPOP0        IFEQ      'P04'
116400070122     C                   MOVEL     DEC(4)        VIDING
116500970526E   1C                   ENDIF
116600970526     C*
116700070119     C                   ENDSR
116800070905      *********************************************************************
116900070905      * Recupero £6 e capofila
117000070905      *********************************************************************
117100070905     c     Recfil        begsr
117200070905
117300070905     c                   clear                   fil
117400070904      * Reperimento capofila £6
117500070905     c                   clear                   FNLV55ds
117600070904     c                   eval      D55tpt  =  '6'
117700070904     c                   eval      D55lin  =  VI1FGS
117800070905     c                   eval      D55drf  =  oggi_aammgg
117900070904     c                   call      'FNLV55R'
118000070904     c                   parm                    FNLV55ds
118100070904if  2c                   if        D55err  <> *blanks
118200070904     c                   eval      D55tfa  =  VI1FGS
118300070904e   2c                   endif
118400070905     c                   eval      fil(1) = d55tfa
118500070905
118600070904      * Reperimento filiali gestione Arrivi (£6)
118700070905
118800070905     c                   clear                   TRUL06ds
118900070905     c                   eval      D06cod   = '£6'
119000070905     c                   movel(p)  Vi1fgs        D06key
119100070905     c                   movel(p)  TRUL06ds      KPJBU
119200070905     c                   call      'TRUL06R'
119300070905     c                   parm                    KPJBA
119400070905     c                   movel     KPJBU         TRUL06ds
119500070905     c                   movea     D06lin        fil(2)
119600070905
119700070904     c                   endsr
119800970526     C*------------------------------------------------------------------------*
119900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
120200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
120300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
120400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120600970526     C*------------------------------------------------------------------------*
120700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
120800970526     C*------------------------------------------------------------------------*
120900970526     C     POSSFL        BEGSR
121000970526     C*
121100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
121200970526     C                   EXSR      CLRSFL
121300970526     C*
121400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
121500970526     C                   EXSR      SETFIL
121600970526     C*
121700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
121800970526     C     SFLEOF        IFEQ      0
121900970526     C                   EXSR      CARSFL
122000970526     C                   END
122100970526     C*
122200970526     C                   ENDSR
122300970526     C*------------------------------------------------------------------------*
122400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
122500970526     C*------------------------------------------------------------------------*
122600970526     C     ENTATT        BEGSR
122700970526     C*
122800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
122900970526     C                   Z-ADD     0             OPZOK             1 0
123000970526     C*
123100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
123200970526     C                   Z-ADD     0             SFLVAR            5 0
123300970526     C*
123400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
123500970526     C                   Z-ADD     0             SFLERR
123600970526     C*
123700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
123800970526     C                   Z-ADD     SFLULT        WFLULT
123900970526     C*
124000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
124100070119     C                   READC     DG09D05                                98
124200970526     C     *IN98         DOWEQ     '0'
124300970526     C*
124400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
124500970526     C                   ADD       1             SFLVAR
124600970526     C*
124700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
124800970526     C     OPZOK         IFEQ      0
124900970526     C*
125000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
125100970526     C     OPZKEY        IFNE      *BLANKS
125200970526     C                   EXSR      OPZCON
125300970526     C                   END
125400970526     C*
125500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
125600970526     C*            2. PULIZIA SCELTA IN SFL
125700970526     C     OPZOK         IFEQ      0
125800970526     C                   MOVEL     '0'           *IN41
125900970526     C                   MOVEL     *BLANKS       OPZKEY
126000970526     C                   ELSE
126100970526     C*
126200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
126300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
126400970526     C                   MOVEL     '1'           *IN41
126500970526     C                   Z-ADD     SFLULT        SFLERR
126600970526     C                   END
126700970526     C                   ELSE
126800970526     C*
126900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
127000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
127100970526     C                   MOVEL     '1'           *IN41
127200970526     C                   END                                                    OPZOK=0
127300970526     C*
127400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
127500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
127600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
127700970526     C     OPZOK         IFNE      0
127800970526     C                   Z-ADD     SFLERR        SFLNBR
127900970526     C                   ELSE
128000970526     C                   Z-ADD     SFLULT        SFLNBR
128100970526     C                   END
128200970526     C*
128300970526     C* AGGIORNO RECORD SFL
128400070906     C*
128500070906     C* Se giro annullato metto in reverse il codice giro stesso
128600070906     C                   if        vihatb <> *blanks
128700070906     C                   seton                                        71
128800070906     C                   endif
128900070906     C*
129000070119     C                   UPDATE    DG09D05
129100970526     C*
129200070119     C                   READC     DG09D05                                98
129300970526     C                   END                                                    DO WHILE
129400970526     C*
129500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
129600970526     C                   Z-ADD     WFLULT        SFLULT
129700970526     C*
129800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
129900970526     C     SFLVAR        IFEQ      0
130000970526     C*---                 MOVEL'1'       FINL00
130100970526     C                   END
130200970526     C*
130300970526     C                   ENDSR
130400970526     C*------------------------------------------------------------------------*
130500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
130600970526     C*------------------------------------------------------------------------*
130700970526     C     CLRSFL        BEGSR
130800970526     C*
130900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
131000970526     C* (0=NON FINE FILE  1=FINE FILE)
131100970526     C                   Z-ADD     0             SFLEOF
131200970526     C*
131300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
131400970526     C                   Z-ADD     0             SFLULT
131500970526     C*
131600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
131700970526     C                   MOVEL     '1'           *IN40
131800070119     C                   WRITE     DG09D06
131900970526     C                   MOVEL     '0'           *IN40
132000970526     C*
132100970526     C                   ENDSR
132200970526     C*------------------------------------------------------------------------*
132300970526     C* CARSFL - CARICAMENTO SFL
132400970526     C*------------------------------------------------------------------------*
132500970526     C     CARSFL        BEGSR
132600970526     C*
132700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
132800970526     C     SFLULT        IFEQ      0
132900970526     C                   EXSR      SETFIL
133000970526     C                   END
133100970526     C*
133200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
133300970526     C                   Z-ADD     SFLULT        WFLULT
133400970526     C*
133500970526     C* CICLO:
133600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
133700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
133800970526     C                   Z-ADD     1             SFLRIG
133900070205     C**** SFLRIG        DOWLE     SFLPAG
134000070205     C**** SFLEOF        ANDEQ     0
134100070205     C                   DOW       (SFLRIG <= SFLPAG OR *IN60 = *ON) AND
134200070205     C                             SFLEOF  = *ZEROS
134300970526     C*
134400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
134500970526     C                   MOVEL     *BLANKS       OPZKEY
134600970526     C*
134700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
134800070124     C                   EXSR      IRESFL
134900970526     C*
135000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
135100970526     C                   ADD       1             SFLULT
135200970526     C*
135300970526     C* SCRITTURA RECORD SFL
135400070124     C  N94              WRITE     DG09D05
135500970526     C                   ADD       1             SFLRIG
135600970526     C*
135700970526     C* LETTURA FILE DA CARICARE
135800970526     C                   EXSR      LETFIL
135900970526     C                   END                                                    DO WHILE
136000970526     C*
136100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
136200970526     C     WFLULT        IFNE      SFLULT
136300970526     C                   Z-ADD     WFLULT        SFLNBR
136400970526     C                   ADD       1             SFLNBR
136500970526     C                   END
136600970526     C*
136700970526     C                   ENDSR
136800970526     C*------------------------------------------------------------------------*
136900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
137000970526     C*------------------------------------------------------------------------*
137100970526     C     OUTSFL        BEGSR
137200970526     C*
137300970526     C* WRITE RECORD TESTATA VIDEO
137400070119     C                   WRITE     DG09D01
137500970526     C*
137600970526     C* VISUALIZZAZIONE MESSAGGIO
137700970526     C                   EXSR      SNDMSG
137800970526     C*
137900970526     C* WRITE RECORD OPZIONI VIDEO
138000070119     C                   WRITE     DG09D02
138100970526     C*
138200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
138300070119     C                   WRITE     DG09D03
138400970526     C*
138500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
138600070119     C                   WRITE     DG09D04
138700970526     C*
138800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
138900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
139000970526     C     SFLEOF        IFEQ      0
139100970526     C                   MOVEL     '0'           *IN42
139200970526     C                   ELSE
139300970526     C*
139400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
139500970526     C                   MOVEL     '1'           *IN42
139600970526     C                   END
139700970526     C*
139800970526     C* WRITE RECORD TASTI FUNZIONALI
139900070119     C                   WRITE     DG09D09
140000970526     C*
140100970526     C* CASO DI SFL VUOTO
140200970526     C     SFLULT        IFLE      0
140300970526     C*
140400970526     C*    - WRITE RECORD VUOTO
140500070323     C                   WRITE     DG09D0A
140600070119     C                   READ      DG09D0A                                99
140700070323     C*    - TRACCIO NN TROVATO NESSUN DATO RISPONDENTE A RICHIESTE
140800070323     C                   MOVEL     '1'           d09OERR
140900070323     C                   MOVEL     MSG(6)        d09OMSG
141000970526     C*
141100970526     C                   ELSE
141200970526     C*
141300970526     C* CASO DI SFL CON RECORDS
141400970526     C*
141500970526     C*    - WRITE RECORD CONTROLLO SFL
141600070119     C                   WRITE     DG09D06
141700070119     C                   READ      DG09D06                                99
141800970526     C                   END
141900970526     C*
142000970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
142100070119     C                   READ      DG09D03                                99
142200970526     C*
142300970526     C                   ENDSR
142400970526     C*------------------------------------------------------------------------*
142500970526     C* SNDMSG - MANDA MESSAGGIO
142600970526     C*------------------------------------------------------------------------*
142700970526     C     SNDMSG        BEGSR
142800970526     C*
142900970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
143000970526     C                   MOVEL     '1'           *IN40
143100070119     C                   WRITE     DG09DMC
143200970526     C                   MOVEL     '0'           *IN40
143300970526     C*
143400970526     C* VISUALIZZAZIONE MESSAGGIO
143500970526     C                   Z-ADD     0             MSGULT
143600970526     C                   CALL      'XMSG'        XMSGPA
143700970526     C     DSMSER        IFEQ      0
143800970526     C                   MOVEL     DSMSKE        MSGKEY
143900970526     C                   END
144000970526     C                   Z-ADD     1             MSGULT
144100070119     C                   WRITE     DG09DMS                                      SFL MESS
144200070119     C                   WRITE     DG09DMC                                      CONTR MESS
144300970526     C*
144400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
144500070905     C                   MOVEL     *BLANKS       DSMSMS
144600970526     C*
144700970526     C                   ENDSR
144800970526     C*------------------------------------------------------------------------*
144900970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
145000970526     C*------------------------------------------------------------------------*
145100970526     C     FUNCON        BEGSR
145200970526     C*
145300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
145400970526     C                   EXSR      FUNVAR
145500970526     C*
145600970526     C* PULISCO CAMPI DI LAVORO
145700970526     C                   Z-ADD     0             I                 5 0
145800970526     C                   Z-ADD     0             W                 5 0
145900970526     C*
146000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
146100970526     C     FUNKEY        IFNE      *BLANKS
146200970526     C     1             DO        24            I
146300970526     C                   MOVEL     FUD(I)        FUDRIG
146400970526     C*
146500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
146600970526     C     FUNKEY        IFEQ      FUDFUN
146700970526     C     FUDATT        ANDEQ     'S'
146800970526     C                   Z-ADD     I             W
146900970526     C                   END
147000970526     C                   END                                                    DO WHILE
147100970526     C*
147200970526     C* FUNZIONE NON ATTIVA (ERRORE)
147300970526     C     W             IFEQ      0
147400970526     C*
147500970526     C* EMISSIONE MESSAGGIO DI ERRORE
147600970526     C                   MOVEL     MSG(2)        DSMSMS
147700970526     C                   END                                                    W=0
147800970526     C                   END                                                    FUNKEY NE *BLANKS
147900970526     C*
148000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
148100970526     C     W             IFNE      0
148200970526     C     FUNKEY        OREQ      *BLANKS
148300970526     C*
148400970526     C     FUNKEY        IFEQ      *BLANKS
148500970526     C                   EXSR      ENTGES
148600970526     C                   END
148700970526     C     FUNKEY        IFEQ      '03'
148800970526     C                   EXSR      F03GES
148900970526     C                   END
149000970526     C     FUNKEY        IFEQ      '05'
149100970526     C                   EXSR      F05GES
149200970526     C                   END
149300970526     C     FUNKEY        IFEQ      '06'
149400970526     C                   EXSR      F06GES
149500970526     C                   END
149600070202     C     FUNKEY        IFEQ      '08'
149700070202     C                   EXSR      F08GES
149800070202     C                   END
149900070119     C     FUNKEY        IFEQ      '10'
150000070119     C                   EXSR      F10GES
150100070119     C                   END
150200070119     C     FUNKEY        IFEQ      '11'
150300070119     C                   EXSR      F11GES
150400070119     C                   END
150500970526     C     FUNKEY        IFEQ      '12'
150600970526     C                   EXSR      F12GES
150700970526     C                   END
150800970528     C     FUNKEY        IFEQ      '21'
150900970528     C                   EXSR      F21GES
151000970528     C                   END
151100970528     C     FUNKEY        IFEQ      '22'
151200970528     C                   EXSR      F22GES
151300970528     C                   END
151400970526     C     FUNKEY        IFEQ      '23'
151500970526     C                   EXSR      F23GES
151600970526     C                   END
151700970526     C     FUNKEY        IFEQ      '24'
151800970526     C                   EXSR      F24GES
151900970526     C                   END
152000970526     C     FUNKEY        IFEQ      'RU'
152100970526     C                   EXSR      RUPGES
152200970526     C                   END
152300970526     C*
152400970526     C                   END
152500970526     C*
152600970526     C                   ENDSR
152700970526     C*------------------------------------------------------------------------*
152800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
152900970526     C*------------------------------------------------------------------------*
153000970526     C     FUNVAR        BEGSR
153100970526     C*
153200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
153300970526     C     KEY           IFEQ      ENTER
153400970526     C                   MOVEL     *BLANKS       FUNKEY
153500970526     C                   END
153600970526     C     KEY           IFEQ      F1
153700970526     C                   MOVEL     '01'          FUNKEY
153800970526     C                   END
153900970526     C     KEY           IFEQ      F2
154000970526     C                   MOVEL     '02'          FUNKEY
154100970526     C                   END
154200970526     C     KEY           IFEQ      F3
154300970526     C                   MOVEL     '03'          FUNKEY
154400970526     C                   END
154500970526     C     KEY           IFEQ      F4
154600970526     C                   MOVEL     '04'          FUNKEY
154700970526     C                   END
154800970526     C     KEY           IFEQ      F5
154900970526     C                   MOVEL     '05'          FUNKEY
155000970526     C                   END
155100970526     C     KEY           IFEQ      F6
155200970526     C                   MOVEL     '06'          FUNKEY
155300970526     C                   END
155400970526     C     KEY           IFEQ      F7
155500970526     C                   MOVEL     '07'          FUNKEY
155600970526     C                   END
155700970526     C     KEY           IFEQ      F8
155800970526     C                   MOVEL     '08'          FUNKEY
155900970526     C                   END
156000970526     C     KEY           IFEQ      F9
156100970526     C                   MOVEL     '09'          FUNKEY
156200970526     C                   END
156300970526     C     KEY           IFEQ      F10
156400970526     C                   MOVEL     '10'          FUNKEY
156500970526     C                   END
156600970526     C     KEY           IFEQ      F11
156700970526     C                   MOVEL     '11'          FUNKEY
156800970526     C                   END
156900970526     C     KEY           IFEQ      F12
157000970526     C                   MOVEL     '12'          FUNKEY
157100970526     C                   END
157200970526     C     KEY           IFEQ      F13
157300970526     C                   MOVEL     '13'          FUNKEY
157400970526     C                   END
157500970526     C     KEY           IFEQ      F14
157600970526     C                   MOVEL     '14'          FUNKEY
157700970526     C                   END
157800970526     C     KEY           IFEQ      F15
157900970526     C                   MOVEL     '15'          FUNKEY
158000970526     C                   END
158100970526     C     KEY           IFEQ      F16
158200970526     C                   MOVEL     '16'          FUNKEY
158300970526     C                   END
158400970526     C     KEY           IFEQ      F17
158500970526     C                   MOVEL     '17'          FUNKEY
158600970526     C                   END
158700970526     C     KEY           IFEQ      F18
158800970526     C                   MOVEL     '18'          FUNKEY
158900970526     C                   END
159000970526     C     KEY           IFEQ      F19
159100970526     C                   MOVEL     '19'          FUNKEY
159200970526     C                   END
159300970526     C     KEY           IFEQ      F20
159400970526     C                   MOVEL     '20'          FUNKEY
159500970526     C                   END
159600970526     C     KEY           IFEQ      F21
159700970526     C                   MOVEL     '21'          FUNKEY
159800970526     C                   END
159900970526     C     KEY           IFEQ      F22
160000970526     C                   MOVEL     '22'          FUNKEY
160100970526     C                   END
160200970526     C     KEY           IFEQ      F23
160300970526     C                   MOVEL     '23'          FUNKEY
160400970526     C                   END
160500970526     C     KEY           IFEQ      F24
160600970526     C                   MOVEL     '24'          FUNKEY
160700970526     C                   END
160800970526     C     KEY           IFEQ      ROLLUP
160900970526     C                   MOVEL     'RU'          FUNKEY
161000970526     C                   END
161100970526     C     KEY           IFEQ      HELP
161200970526     C                   MOVEL     'HP'          FUNKEY
161300970526     C                   END
161400970526     C*
161500970526     C                   ENDSR
161600970526     C*------------------------------------------------------------------------*
161700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
161800970526     C*------------------------------------------------------------------------*
161900970526     C     FUNGES        BEGSR
162000970526     C*
162100970526     C* PULISCO CAMPI DI LAVORO
162200970526     C                   Z-ADD     0             W                 5 0
162300970526     C*
162400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
162500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
162600970526     C     1             DO        24            I
162700970526     C                   MOVEL     FUD(I)        FUDRIG
162800970526     C*
162900970526     C* PULIZIA FUNZIONI PRECEDENTI
163000970526     C                   MOVEL     *BLANKS       FUV(I)
163100970526     C*
163200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
163300970526     C* (SALTA F23 E F24)
163400970526     C     FUDATT        IFEQ      'S'
163500970526     C     FUDFUN        ANDNE     '23'
163600970526     C     FUDFUN        ANDNE     '24'
163700970526     C                   ADD       1             W
163800970526     C                   MOVEL     FUDDES        FUV(W)
163900970526     C                   END
164000970526     C                   END                                                    DO WHILE
164100970526     C*
164200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
164300970526     C* ALTRIMENTI DISATTIVO F24
164400970526     C     1             DO        24            I
164500970526     C                   MOVEL     FUD(I)        FUDRIG
164600970526     C*
164700970526     C     FUDFUN        IFEQ      '24'
164800970526     C     W             IFGT      6
164900970526     C                   MOVEL     'S'           FUDATT
165000970526     C                   ELSE
165100970526     C                   MOVEL     *BLANKS       FUDATT
165200970526     C                   END                                                    W GT 8
165300970526     C                   MOVEL     FUDRIG        FUD(I)
165400970526     C                   END
165500970526     C                   END                                                    DO WHILE
165600970526     C*
165700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
165800970526     C     W             DIV       6             I
165900970526     C                   MVR                     Z
166000970526     C*
166100970526     C     Z             IFNE      0
166200970526     C     I             ADD       1             Z
166300970526     C                   ELSE
166400970526     C                   Z-ADD     I             Z
166500970526     C                   END
166600970526     C*
166700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
166800970526     C                   ADD       1             FUNGRU
166900970526     C     FUNGRU        IFGT      Z
167000970526     C     FUNGRU        ORLT      1
167100970526     C                   Z-ADD     1             FUNGRU
167200970526     C                   END
167300970526     C*
167400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
167500970526     C     FUNGRU        IFEQ      1
167600970526     C                   MOVEL     FUV(1)        FUN001
167700970526     C                   MOVEL     FUV(2)        FUN002
167800970526     C                   MOVEL     FUV(3)        FUN003
167900970526     C                   MOVEL     FUV(4)        FUN004
168000970526     C                   MOVEL     FUV(5)        FUN005
168100970526     C                   MOVEL     FUV(6)        FUN006
168200970526     C                   END
168300970526     C*
168400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
168500970526     C     FUNGRU        IFEQ      2
168600970526     C                   MOVEL     FUV(7)        FUN001
168700970526     C                   MOVEL     FUV(8)        FUN002
168800970526     C                   MOVEL     FUV(9)        FUN003
168900970526     C                   MOVEL     FUV(10)       FUN004
169000970526     C                   MOVEL     FUV(11)       FUN005
169100970526     C                   MOVEL     FUV(12)       FUN006
169200970526     C                   END
169300970526     C*
169400970526     C* TERZO GRUPPO DI 6 FUNZIONI
169500970526     C     FUNGRU        IFEQ      3
169600970526     C                   MOVEL     FUV(13)       FUN001
169700970526     C                   MOVEL     FUV(14)       FUN002
169800970526     C                   MOVEL     FUV(15)       FUN003
169900970526     C                   MOVEL     FUV(16)       FUN004
170000970526     C                   MOVEL     FUV(17)       FUN005
170100970526     C                   MOVEL     FUV(18)       FUN006
170200970526     C                   END
170300970526     C*
170400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
170500970526     C     FUNGRU        IFEQ      4
170600970526     C                   MOVEL     FUV(19)       FUN001
170700970526     C                   MOVEL     FUV(20)       FUN002
170800970526     C                   MOVEL     FUV(21)       FUN003
170900970526     C                   MOVEL     FUV(22)       FUN004
171000970526     C                   MOVEL     *BLANKS       FUN005
171100970526     C                   MOVEL     *BLANKS       FUN006
171200970526     C                   END
171300970526     C*
171400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
171500970526     C                   MOVEL     *BLANKS       FUN007
171600970526     C                   MOVEL     *BLANKS       FUN008
171700970526     C*
171800970526     C     1             DO        24            I
171900970526     C                   MOVEL     FUD(I)        FUDRIG
172000970526     C     FUDFUN        IFEQ      '23'
172100970526     C     FUDATT        ANDEQ     'S'
172200970526     C                   MOVEL     FUDDES        FUN007
172300970526     C                   END
172400970526     C     FUDFUN        IFEQ      '24'
172500970526     C     FUDATT        ANDEQ     'S'
172600970526     C                   MOVEL     FUDDES        FUN008
172700970526     C                   END
172800970526     C                   END                                                    DO
172900970526     C*
173000970526     C                   ENDSR
173100970526     C*------------------------------------------------------------------------*
173200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
173300970526     C*------------------------------------------------------------------------*
173400970526     C     OPZGES        BEGSR
173500970526     C*
173600970526     C* PULISCO CAMPI DI LAVORO
173700970526     C                   Z-ADD     0             W                 5 0
173800970526     C*
173900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
174000970526     C* E CARICO TABELLA OPZIONI ATTIVE
174100970526     C     1             DO        24            I
174200970526     C                   MOVEL     OPD(I)        OPDRIG
174300970526     C*
174400970526     C* PULIZIA OPZIONI PRECEDENTI
174500970526     C                   MOVEL     *BLANKS       OPV(I)
174600970526     C*
174700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
174800970526     C     OPDATT        IFEQ      'S'
174900970526     C                   ADD       1             W
175000970526     C                   MOVEL     OPDDES        OPV(W)
175100970526     C                   END
175200970526     C                   END                                                    DO WHILE
175300970526     C*
175400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
175500970526     C* ALTRIMENTI DISATTIVO F23
175600970526     C     1             DO        24            I
175700970526     C                   MOVEL     FUD(I)        FUDRIG
175800970526     C*
175900970526     C     FUDFUN        IFEQ      '23'
176000970526     C     W             IFGT      8
176100970526     C                   MOVEL     'S'           FUDATT
176200970526     C                   ELSE
176300970526     C                   MOVEL     *BLANKS       FUDATT
176400970526     C                   END                                                    W GT 8
176500970526     C                   MOVEL     FUDRIG        FUD(I)
176600970526     C                   END
176700970526     C                   END                                                    DO WHILE
176800970526     C*
176900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
177000970526     C     W             DIV       8             I
177100970526     C                   MVR                     Z                 5 0
177200970526     C*
177300970526     C     Z             IFNE      0
177400970526     C     I             ADD       1             Z
177500970526     C                   ELSE
177600970526     C                   Z-ADD     I             Z
177700970526     C                   END
177800970526     C*
177900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
178000970526     C                   ADD       1             OPZGRU
178100970526     C     OPZGRU        IFGT      Z
178200970526     C     OPZGRU        ORLT      1
178300970526     C                   Z-ADD     1             OPZGRU
178400970526     C                   END
178500970526     C*
178600970526     C* PRIMO GRUPPO DI 8 OPZIONI
178700970526     C     OPZGRU        IFEQ      1
178800970526     C                   MOVEL     OPV(1)        OPZ001
178900970526     C                   MOVEL     OPV(2)        OPZ002
179000970526     C                   MOVEL     OPV(3)        OPZ003
179100970526     C                   MOVEL     OPV(4)        OPZ004
179200970526     C                   MOVEL     OPV(5)        OPZ005
179300970526     C                   MOVEL     OPV(6)        OPZ006
179400970526     C                   MOVEL     OPV(7)        OPZ007
179500970526     C                   MOVEL     OPV(8)        OPZ008
179600970526     C                   END
179700970526     C*
179800970526     C* SECONDO GRUPPO DI 8 OPZIONI
179900970526     C     OPZGRU        IFEQ      2
180000970526     C                   MOVEL     OPV(9)        OPZ001
180100970526     C                   MOVEL     OPV(10)       OPZ002
180200970526     C                   MOVEL     OPV(11)       OPZ003
180300970526     C                   MOVEL     OPV(12)       OPZ004
180400970526     C                   MOVEL     OPV(13)       OPZ005
180500970526     C                   MOVEL     OPV(14)       OPZ006
180600970526     C                   MOVEL     OPV(15)       OPZ007
180700970526     C                   MOVEL     OPV(16)       OPZ008
180800970526     C                   END
180900970526     C*
181000970526     C* TERZO GRUPPO DI 8 OPZIONI
181100970526     C     OPZGRU        IFEQ      3
181200970526     C                   MOVEL     OPV(17)       OPZ001
181300970526     C                   MOVEL     OPV(18)       OPZ002
181400970526     C                   MOVEL     OPV(19)       OPZ003
181500970526     C                   MOVEL     OPV(20)       OPZ004
181600970526     C                   MOVEL     OPV(21)       OPZ005
181700970526     C                   MOVEL     OPV(22)       OPZ006
181800970526     C                   MOVEL     OPV(23)       OPZ007
181900970526     C                   MOVEL     OPV(24)       OPZ008
182000970526     C                   END
182100970526     C*
182200970526     C                   ENDSR
182300970526     C*------------------------------------------------------------------------*
182400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
182500970526     C*------------------------------------------------------------------------*
182600970526     C     OPZCON        BEGSR
182700970526     C*
182800970526     C* PULISCO CAMPI DI LAVORO
182900970526     C                   Z-ADD     0             I                 5 0
183000970526     C                   Z-ADD     0             W                 5 0
183100970526     C*
183200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
183300970526     C     1             DO        24            I
183400970526     C                   MOVEL     OPD(I)        OPDRIG
183500970526     C*
183600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
183700970526     C     OPZKEY        IFEQ      OPDOPZ
183800970526     C     OPZKEY        OREQ      OPDOP1
183900970526     C*
184000970526     C     OPDATT        IFEQ      'S'
184100970526     C                   MOVEL     OPDOPZ        OPZKEY
184200970526     C                   Z-ADD     I             W
184300970526     C                   END
184400970526     C                   END
184500970526     C*
184600970526     C                   END                                                    DO WHILE
184700970526     C*
184800970526     C* OPZIONE NON ATTIVA (ERRORE)
184900970526     C     W             IFEQ      0
185000970526     C*
185100970526     C* OPZIONE NON OK
185200970526     C                   Z-ADD     1             OPZOK
185300970526     C*
185400970526     C* EMISSIONE MESSAGGIO DI ERRORE
185500970526     C                   MOVEL     MSG(1)        DSMSMS
185600970526     C                   ELSE
185700970526     C*
185800970526     C* OPZIONE ATTIVA (PROCEDI ...)
185900970526     C*
186000970526     C     OPZKEY        IFEQ      ' 1'
186100970526     C                   EXSR      O01GES
186200970526     C                   END
186300970526     C     OPZKEY        IFEQ      ' 2'
186400970526     C                   EXSR      O02GES
186500970526     C                   END
186600991014     C     OPZKEY        IFEQ      ' 3'
186700991014     C                   EXSR      O03GES
186800991014     C                   END
186900070119     C     OPZKEY        IFEQ      ' 4'
187000070119     C                   EXSR      O04GES
187100070119     C                   END
187200970526     C     OPZKEY        IFEQ      ' 5'
187300970526     C                   EXSR      O05GES
187400970526     C                   END
187500970526     C     OPZKEY        IFEQ      ' 7'
187600970526     C                   EXSR      O07GES
187700970526     C                   END
187800970526     C                   END                                                    OPZ.NON ATTIVA
187900970526     C*
188000970526     C                   ENDSR
188100970526     C*------------------------------------------------------------------------*
188200970526     C* NOTE COSTRUZIONE SCHIERE
188300970526     C*------------------------------------------------------------------------*
188400970526     C*
188500970526     C* -MSG-MESSAGGI
188600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
188700970526     C* (NON TOCCARE MSG,1 E MSG,2)
188800970526     C*
188900970526     C* -CMD-COMANDI
189000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
189100970526     C*
189200970526     C* -OPD-OPZIONI
189300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
189400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
189500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
189600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
189700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
189800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
189900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
190000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
190100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
190200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
190300970526     C*
190400970526     C* -FUD-FUNZIONI
190500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
190600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
190700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
190800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
190900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
191000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
191100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
191200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
191300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
191400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
191500970526     O*------------------------------------------------------------------------*
191600970526** DEC - DECODIFICHE
191700070119     SCELTA                                                      1
191800070122 INTERROGAZIONE                                                  2
191900070122    GESTIONE                                                     3
192000070122  GESTIONE FULL                                                  4
192100070122                                                                 5
192200970526** MSG - MESSAGGI
192300070119FIDG09R- Opzione non gestita                                                   1
192400070119FIDG09R- Tasto funzionale non gestito                                          2
192500070119FIDG09R- 3                                                                     3
192600070119FIDG09R- Record annullato. Consentito solo il ripristino                       4
192700070119FIDG09R- Record non annullato. Impossibile ripristinare                        5
192800070124FIDG09R- Nessun record corrispondente ai requisiti richiesti                   6
192900070124FIDG09R- Trovati troppi record corrispondenti ai requisiti richiesti           7
193000070905FIDG09R- Filiale errata                                                        8
193100070119FIDG09R- 9                                                                     9
193200070119FIDG09R- 10                                                                   10
193300070119FIDG09R- 11                                                                   11
193400070119FIDG09R- Data errata                                                          12
193500070119FIDG09R- 13                                                                   13
193600070119FIDG09R- 14                                                                   14
193700070119FIDG09R- 15                                                                   15
193800070119FIDG09R- 16                                                                   16
193900070119FIDG09R- 17                                                                   17
194000070119FIDG09R- 18                                                                   18
194100070119FIDG09R- 19                                                                   19
194200070119FIDG09R- 20                                                                   20
194300970528** CMD - COMANDI CL
194400970528WRKSPLF
194500970528
194600970528
194700970528
194800970528
194900970526** OPD - OPZIONI
195000970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
195100970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
195200991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
195300070119 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
195400970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
195500000703                   -6 -     (S/N)=N                        opz. 06¬
195600970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
195700970527                   -8 -     (S/N)=N                        opz. 08¬
195800070119                   -9 -     (S/N)=N                        opz. 09¬
195900070119                   -10-     (S/N)=N                        opz. 10¬
196000070119                   -11-     (S/N)=N                        opz. 11¬
196100970526                   -  -     (S/N)=N                        opz. 12¬
196200970526                   -  -     (S/N)=N                        opz. 13¬
196300970526                   -  -     (S/N)=N                        opz. 14¬
196400970526                   -  -     (S/N)=N                        opz. 15¬
196500970526                   -  -     (S/N)=N                        opz. 16¬
196600970526                   -  -     (S/N)=N                        opz. 17¬
196700970526                   -  -     (S/N)=N                        opz. 18¬
196800970526                   -  -     (S/N)=N                        opz. 19¬
196900970526                   -  -     (S/N)=N                        opz. 20¬
197000970526                   -  -     (S/N)=N                        opz. 21¬
197100970526                   -  -     (S/N)=N                        opz. 22¬
197200970526                   -  -     (S/N)=N                        opz. 23¬
197300970526                   -  -     (S/N)=N                        opz. 24¬
197400970526** FUD - FUNZIONI
197500970526                   -        (S/N)=N                        funz.01¬
197600970526                   -        (S/N)=N                        funz.02¬
197700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
197800970526                   -        (S/N)=N                        funz.04¬
197900970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
198000970526F06=Inserimento    -        (S/N)=N                        funz.06¬
198100970526                   -        (S/N)=N                        funz.07¬
198200070205F08=Stampa elenco  -        (S/N)=S  <== S-FISSO           funz.08¬
198300970527                   -        (S/N)=N                        funz.09¬
198400070119F10=Ordina x cod.  -        (S/N)=S                        funz.10¬
198500070119F11=Ordina x desc. -        (S/N)=S                        funz.11¬
198600970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
198700970526                   -        (S/N)=N                        funz.13¬
198800970526                   -        (S/N)=N                        funz.14¬
198900970526                   -        (S/N)=N                        funz.15¬
199000970526                   -        (S/N)=N                        funz.16¬
199100970526                   -        (S/N)=N                        funz.17¬
199200970526                   -        (S/N)=N                        funz.18¬
199300970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
199400970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
199500070205F21=Stampa elenco  -        (S/N)=N                        funz.21¬
199600970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
199700970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
199800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
