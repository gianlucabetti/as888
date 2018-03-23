000100940222     H DECEDIT('0,') DATEDIT(*DMY.)
000200060207     H* FNLV85R  *----------------------------------------------------*
000300930406     H*------------------*                                            *
000400060207     H*        GESTIONE MOTIVO VARIAZIONE BOLLA                      **
000500921214     H*---------------------------------------------------------------*
000600060227     F****tabel00f  iF   E           K DISK
000700060209     Ffiar500f  O  A E             DISK
000800060207     FFNLV85D   CF   E             WORKSTN
000900921214     D*
001000060207     D msg             S             50    DIM(2) CTDATA PERRCD(1)
001100060207
001200060207     D fnlv85ds      e DS
001300940218     D*
001400060227     d*DS7L          E DS
001500060207     D Dmtv          E DS
001600921214     D KPJBA         E DS
001700060207     D tibs02ds      E DS
001800060209     D dar5mtv       E DS
001900060209     D***
002000060209     D WLBDAT          DS                  INZ
002100060209     D  G02DAT                 1      8  0
002200060209     D  G02INV                 9     16  0
002300060209     D  G02ERR                17     17
002400060209     D  G02TGI                18     22  0
002500060207     d*
002600060227     D*codut           S                   LIKE(TBLkut) inz(1)
002700060227     D*kcod            S                   LIKE(TBLcod) inz('7L')
002800060227     D*kkey            S                   LIKE(TBLkey)
002900921214     I*
003000940209     C*****************************************************************
003100940209     C* RIEPILOGO INDICATORI
003200940209     C*****************************************************************
003300940209     C*****************************************************************
003400940209     C     *ENTRY        PLIST
003500060207     C                   PARM                    kpjba
003600060227     C****               PARM                    ds7l
003700060207     c*
003800060207     c                   movel     kpjbu         fnlv85ds
003900060209     c*
004000060209     C                   TIME                    W0140            14 0
004100060209     C* UDATE IN GGMMAAAA
004200060209     C                   MOVE      W0140         WDTGIO            8 0
004300060209     C*
004400060209     C* UDATE IN AAAAMMGG
004500060209     C                   Z-ADD     WDTGIO        G02DAT
004600060209     C                   MOVEL     *BLANK        G02ERR
004700060209     C                   CALL      'XSRDA8'
004800060209     C                   PARM                    WLBDAT
004900060209     C                   MOVEL     G02INV        DATEU             8 0
005000060207     c
005100940210     C*
005200940209     C*---------------------------------------------------------------*
005300940209     C*
005400060207     C* ACCESSO Tabel00F
005500060227     C***  KTab          KLIST
005600060227     C***                KFLD                    CODUT
005700060227     C***                KFLD                    kcod
005800060227     C***                KFLD                    Kkey
005900940210     C*
006000921214     C*---------------------------------------------------------------*
006100060207      *    tla = ' '  Elaboro e chiudo con RETURN
006200060207      *    tla = 'L'  Elaboro e chiudo con LR
006300060207      *    tla = 'C'            chiudo con LR  solo per chiusura file
006400021022      *
006500060207    1c                   if        Ilv85tla = 'C'
006600060207     c                   eval      *inlr = *on
006700060207
006800060207   X1c                   else
006900060207     c*
007000060207     c* pulizia campi di output
007100060207     c                   clear                   olv85mtv
007200060207     c                   clear                   olv85nt1
007300060207     c                   clear                   olv85nt2
007400060207     c                   clear                   olv85f12
007500060224     c                   clear                   olv85err
007600060209     c*
007700060209     c                   select
007800060209     c                   when      ilv85tel = 'V'
007900060209     c                   exsr      gesvid
008000060209     c                   when      ilv85tel = 'R'
008100060209     c                   exsr      geswrt
008200060209     c                   endsl
008300060207     c*
008400921214     C*
008500060207     c                   movel     fnlv85ds      kpjbu
008600060207    2c                   if        Ilv85tla = 'L'
008700060207     c                   eval      *inlr = *on
008800060207   X2c                   else
008900060207     c                   return
009000060207    2c                   endif
009100940222     C*
009200060207    1c                   endif
009300060207     C*****************************************************************
009400060207     c     *inzsr        begsr
009500060207     C*****************************************************************
009600060207     c*
009700060207     c* Pulizia campi del video (solo la prima volta per mantenere i dati
009800060207     c* immessi fino a nuova bolla). Quindi a fine bolla chiudere sempre
009900060207     c* questo pgm con LR
010000060207     c                   move      '  ?'         d01mtv
010100060207     c                   clear                   d01dmt
010200060207     c                   clear                   d01nt1
010300060207     c                   clear                   d01nt2
010400060207     c                   endsr
010500060209     C*****************************************************************
010600060209     c     gesvid        begsr
010700060209     C*****************************************************************
010800060209     c* con la causale ricevuta chaino tabella "7L" per verificare
010900060209     c* se richiesto il motivo variazione
011000060227     c****               movel(P)  ilv85cvb      kkey
011100060227     c****               clear                   ds7l
011200060227     c**** ktab          chain     tabel00f
011300060227     c****               if        %found(tabel00f)
011400060227     c****               movel     tbluni        ds7l
011500060227     c***                endif
011600060209     C*
011700060227    2c****               if        §7lmtv='S'
011800060224     c* se ricevuta motivazione dal chiamante valorizzo campi del video
011900060224     c                   if        ilv85mtv <> *blanks
012000060224     c                   move      ilv85mtv      d01mtv
012100060224     c                   move      ilv85nt1      d01nt1
012200060224     c                   move      ilv85nt2      d01nt2
012300060224     c                   endif
012400060309     c* imposto riga/colonna window
012500060309     c                   z-add     3             row
012600060309     c                   z-add     23            col
012700060309     c                   if        ilv85row > 0
012800060309     c                   z-add     ilv85row      row
012900060309     c                   endif
013000060309     c                   if        ilv85col > 0
013100060309     c                   z-add     ilv85col      col
013200060309     c                   endif
013300060301     c                   if        ilv85wdw = *blanks
013400060301     C     FMT00         TAG
013500060209     C* EMISSIONE FORMATO
013600060209     C                   WRITE     LV85D02
013700060209     C     FMT01         TAG
013800060209     C                   EXFMT     LV85D01
013900060209     c                   setoff                                       9028
014000060209     c                   clear                   d01msg
014100060209     C*
014200060209     C** CMD12 - FINE LAVORO
014300060209     c   kl              eval      olv85f12 = '1'
014400060209     C   KL              GOTO      Fgesvid
014500060209     C*
014600060209     c                   exsr      ctrd01
014700060301     c                   if        ilv85wdw = *blanks
014800060209     c   90
014900060209     cornkf              goto      fmt01
015000060301     c                   else
015100060301     c   90              goto      fmt01
015200060301     c                   endif
015300060209     c*
015400060209     C     fgesvid       TAG
015500060224     c                   else
015600060224     c                   exsr      ctrd01
015700060301     c                   if        *in90 = *on and ilv85wdw = 'E'
015800060301     c                   goto      fmt00
015900060301     c                   endif
016000060224     c   90              movel     '1'           olv85err
016100060224     c                   endif
016200060209     c                   movel     d01mtv        olv85mtv
016300060209     c                   movel     d01nt1        olv85nt1
016400060209     c                   movel     d01nt2        olv85nt2
016500060209     C*
016600060227    2c****               endif
016700060209     c                   endsr
016800060207     C*****************************************************************
016900060207     c     ctrd01        begsr
017000060207     C*****************************************************************
017100060207     c* codice motivo variazione
017200060207     c                   clear                   d01dmt
017300060207     C* RICERCA
017400060207     C     '?'           SCAN      d01mtv                                 30
017500060207If  2c                   If        *In30
017600060207     c                   Clear                   tibs02ds
017700060207     c                   Eval      T02Mod = 'R'
017800060207     c                   Eval      T02Sif = Knsif
017900060207     c                   Eval      T02Cod = 'MTV'
018000060207     c                   Call      'TIBS02R'
018100060207     c                   Parm                    Kpjba
018200060207     c                   Parm                    tibs02ds
018300060207     c                   Movel     T02Ke1        d01mtv
018400060207     c                   Movel     T02uni        dmtv
018500060207     c                   movel     §mtvdes       d01dmt
018600060207     c                   seton                                        0190
018700060207     C                   WRITE     LV85D02
018800060207     c                   GoTo      ENDc01
018900060207    1C                   ENDIF
019000060207     c* validità
019100060207     c                   if        d01mtv = *blanks
019200060207     c                   seton                                        019028
019300060207     c                   movel     msg(1)        d01msg
019400060207     c                   goto      endc01
019500060207     c                   endif
019600060207     C                   CLEAR                   tibs02ds
019700060207     c                   clear                   dmtv
019800060207     C                   MOVEL     'C'           T02MOD
019900060207     C                   MOVEL     KNSIF         T02SIF
020000060207     C                   MOVEL     'MTV'         T02COD
020100060207     c                   movel(P)  d01mtv        t02ke1
020200060207     C                   CALL      'TIBS02R'
020300060207     C                   PARM                    KPJBA
020400060207     C                   PARM                    tibs02ds
020500060207    2C                   IF        T02ERR = *BLANKS
020600060207     C                   MOVEL     T02uni        dmtv
020700060207     c                   movel     §mtvdes       d01dmt
020800060207     c                   else
020900060207     c                   seton                                        019028
021000060207     c                   movel     msg(1)        d01msg
021100060207     c                   goto      endc01
021200060207    2C                   ENDIF
021300060207     c* note se obbligatorie
021400060207     c                   if        §mtvnob='S' and
021500060207     c                             d01nt1 = *blanks  and d01nt2=*blanks
021600060207     c                   seton                                        029028
021700060207     c                   movel     msg(2)        d01msg
021800060207     c                   goto      endc01
021900060207     c                   endif
022000060207     c
022100060207     c     endc01        tag
022200060207     c                   endsr
022300060209     C*****************************************************************
022400060209     c     geswrt        begsr
022500060209     C*****************************************************************
022600060209     c                   clear                   fiar5000
022700060209     c                   z-add     ilv85aas      ar5aas
022800060209     c                   z-add     ilv85lnp      ar5lnp
022900060209     c                   z-add     ilv85nrs      ar5nrs
023000060209     c                   z-add     ilv85nsp      ar5nsp
023100060209     c                   eval      ar5trd = '§' + ilv85cvb
023200060209     c                   if        ilv85dac <> *zeros
023300060209     c                   z-add     ilv85dac      ar5dac
023400060209     c                   z-add     ilv85orc      ar5orc
023500060209     c                   else
023600060209     c                   z-add     dateu         ar5dac
023700060209     c                   time                    ar5orc
023800060209     c                   endif
023900060209     c                   clear                   dar5mtv
024000060209     c                   if        ilv85mtv <> *blanks
024100060209     c                   eval      §ar5mtv = ilv85mtv
024200060209     c                   eval      §ar5txt = ilv85nt1 + ilv85nt2
024300060209     c                   else
024400060209     c                   eval      §ar5mtv = d01mtv
024500060209     c                   eval      §ar5txt = d01nt1 + d01nt2
024600060209     c                   endif
024700060209     c                   movel     dar5mtv       ar5uni
024800060209     c*
024900060209     c                   write     fiar5000
025000060209     c
025100060209     c                   endsr
025200060207**
025300060207Codice mancante o errato
025400060207Immettere descrizione del motivo della variazione
