000100120712      */*END
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300960116     H* FNLR79R *----------------------------------------------------*
000400000000     H*-----------------*                                            *
000500960116     H*         - ELENCO BOLLE CON OBBLIGO RESTITUZIONE -            *
000600960116     H*            - BAM FIRMATA/LETTERA DI VETTURA -                *
000700940221      *--------------------------------------------------------------*
000800960117      * KC - FINE LAVORO                                             *
000900960117      * KF - CONFERMA                                                *
001000960117      * KS - CAMBIO FILIALE                                          *
001100960117      *--------------------------------------------------------------*
001200960117      * 02 - POSIZIONAMENTO SU FILIALE IN GESTIONE SE DIGITATO F18   *
001300120727      * 03 - Richiesta "Solo bolle NON scannerizzate"                *
001400960117      * 04 - PROTECT FILIALE IN GESTIONE                             *
001500960117      * 05 - ON=ABILIATATO F18/OFF=NON ABILITATO F18                 *
001600960117      * 28 - VISULIZZAZIONE ERRORI                                   *
001700960117      * 30 - CHAIN GENERICHE                                         *
001800960117      * 31 - LOKUP                                                   *
001900960117      * 40 - ERRORE COD.FILIALE IN GESTIONE                          *
002000960116      * 41 - ERRORE SU DATA DI STAMPA                                *
002100120727      * 42 - ERRORE SU RISTAMPA SOLO BOLLE NON SCANNERIZZATE         *
002200940221      *--------------------------------------------------------------*
002300960116     FFNLR79D   CF   E             WORKSTN
002400960116     FFNARBX1L  IF   E           K DISK
002500960116     FTABEL00F  IF   E           K DISK
002600941230      *--------------------------------------------------------------*
002700120706      *   S c h i e r e                                              *
002800960116      *--------------------------------------------------------------*
002900960116     D L£6             S              3  0 DIM(50)                              LINEE £6
003000960116     D £1              S              3  0 DIM(30)                              FIL GESTITE
003100120727     D ERR             S             70    DIM(10) CTDATA PERRCD(1)             ERRORI A VIDEO
003200941230      *--------------------------------------------------------------*
003300960116      *   D s                                                        *
003400960116      *--------------------------------------------------------------*
003500120706     D FNLR80DS      E DS                  INZ
003600970718     D DS5A2         E DS
003700960116     D*  DS  per controllo/conversione data
003800941230     D WLBDA8          DS
003900941230     D  G02DAT                 1      8  0
004000941230     D  G02INV                 9     16  0
004100941230     D  G02ERR                17     17
004200941230     D  G02TGI                18     22  0
004300960117     D WGIDAT          DS
004400960117     D  GI8DAT                 1      8  0
004500960117     D  GI8INV                 9     16  0
004600960117     D  GI8TGI                17     21  0
004700020723      * ds per trul06r
004800020723     d trul06ds      e ds
004900020723     d  lin                    1     90  0 dim(30)
005000920909     D KPJBA         E DS
005100120706     D CNCR80        E DS
005200920909     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300960117     D                SDS
005400120706     D  SDSPGM                 1     10
005500120706      *--------------------------------------------------------------*
005600120706      *   V a r i a b i l i                                          *
005700120706      *--------------------------------------------------------------*
005800120706     D wOk             s               n   inz
005900120730     D $SbmJob         s               n   inz
006000960116      *--------------------------------------------------------------*
006100960116      *   C i c l o        P r i n c i p a l e                       *
006200960116      *--------------------------------------------------------------*
006300960116      *  Inizializzazione prima videata
006400960116     C                   EXSR      INZ01
006500960116      *  Loop gestione videate
006600960116     C     WFINE         DOWNE     'S'
006700960116     C     WTPVID        CASEQ     '1'           GESD01
006800120706     C                   ENDCS
006900120706     C                   ENDDO
007000960116      *  Fine Lavoro
007100960116     C     FINE          TAG
007200960116     C                   SETON                                        LR
007300960116      *--------------------------------------------------------------*
007400960116      *   Gestione prima videata                                     *
007500960116      *--------------------------------------------------------------*
007600960116     C     GESD01        BEGSR
007700960116      *
007800120706     C                   write     LR79T01
007900960116     C                   EXFMT     LR79D01
008000960116     C                   SETOFF                                       404128
008100120727     C                   SETOFF                                       42
008200960116      *  Fine Lavoro
008300960116     C     *INKC         IFEQ      '1'
008400960116     C                   MOVEL     'S'           WFINE
008500960116     C                   GOTO      FINVD1
008600120706     C                   ENDIF
008700960116      * Se ho richiesto il cambio filiale ricarico £6
008800960116     C     *IN02         IFEQ      '1'
008900960116     C                   SETOFF                                       02
009000120706     C                   ENDIF
009100960116      *  Cambio filiale
009200960116     C     *INKS         IFEQ      '1'
009300960116     C                   SETON                                        0402
009400960116     C                   GOTO      FINVD1
009500120706     C                   ENDIF
009600960116     C* Routine controlli
009700960116     C                   EXSR      CTR01
009800960116     C   28              GOTO      FINVD1
009900960116     C* Conferma
010000960116     C     *INKF         IFEQ      '1'
010100960116     C                   EXSR      SBMP80
010200960116     C                   EXSR      INZ01
010300120706     C                   ENDIF
010400960116      *
010500960116     C     FINVD1        ENDSR
010600960116      *--------------------------------------------------------------*
010700960116      *   Controlli prima videata                                    *
010800960116      *--------------------------------------------------------------*
010900960116     C     CTR01         BEGSR
011000960116      *
011100960116      * Se ho modificato la filiale controllo se esiste
011200960116     C     V1CFGS        IFNE      WSVFGS
011300960116     C     V1CFGS        LOOKUP    £1                                     31
011400960116     C     *IN31         IFEQ      '0'
011500960116     C                   SETON                                        2840
011600960116     C                   MOVEL     ERR(1)        $MSG                           FGS non su AS
011700960116     C                   GOTO      FINCT1
011800120706     C                   ENDIF
011900960116     C     V1CFGS        LOOKUP    L£6                                    31
012000960116     C     *IN31         IFEQ      '1'
012100960116     C                   SETON                                        2840
012200960116     C                   MOVEL     ERR(2)        $MSG                           LNA no autonoma
012300960116     C                   GOTO      FINCT1
012400120706     C                   ENDIF
012500960116     C                   MOVE      V1CFGS        WSVFGS            3 0
012600120706     C                   ENDIF
012700120712      *
012800120712if  1C     V1Cris        ifEQ      'N'
012900120727      *
013000120727      *  Controllo data di ristampa
013100120727if  2C     V1Cdts        ifNE      *zero
013200120712     C                   seton                                        2841      data solo
013300120712     C                   eval      $Msg = Err(4)                                x ristampa
013400120727e   2C                   endif
013500120727      *
013600120727      *  Controllo flag ristampa solo bolle NON scannerizzate
013700120727if  2C                   if        V1Csns = 'S'
013800120727     C                   seton                                        2842
013900120727     C                   eval      $Msg = Err(9)
014000120727e   2C                   endif
014100120712      *
014200120727x   1C                   ELSE
014300120712      *
014400120712if  2C     V1CDTS        IFEQ      0
014500120712      *
014600120712     C                   SETON                                        2841      data obbl.
014700120712     C                   MOVEL     ERR(3)        $MSG                           X ristampa
014800120712     C                   leavesr
014900120712      *
015000120712x   2C                   ELSE
015100120712      *
015200120712      *  Controllo se validità data ristampa
015300120712     C                   CLEAR                   WLBDA8
015400120712     C                   Z-ADD     V1CDTS        G02DAT
015500120712     C                   CALL      'XSRDA8'
015600120712     C                   PARM                    WLBDA8
015700120712if  3C     G02ERR        IFEQ      '1'
015800120712     C                   SETON                                        2841      data errata
015900120712     C                   MOVEL     ERR(6)        $MSG
016000120712     C                   leavesr
016100120712e   3C                   ENDIF
016200120712     C                   Z-ADD     G02INV        WDTSTA            8 0
016300120712     C                   Z-ADD     G02DAT        V1CDTS
016400120712if  3C     WDTSTA        IFGT      WOGGI
016500120712     C                   SETON                                        2841      data ristamp
016600120712     C                   MOVEL     ERR(5)        $MSG                           > oggi
016700120712     C                   leavesr
016800120712e   3C                   ENDIF
016900120712if  3C     WDTSTA        IFLE      WDTPUL
017000120712     C                   SETON                                        2841      data ristamp
017100120712     C                   MOVEL     ERR(7)        $MSG                           < data puliz.
017200120712     C                   leavesr
017300120712e   3C                   ENDIF
017400120727      *
017500120727      *  Controllo flag ristampa solo bolle NON scannerizzate
017600120727if  2C                   if        V1Csns = 'S'  and  V1Cfgs <> Wfgs
017700120727     C                   eval      Kcod = 'WS'
017800120727     C                   eval      Kkey = %editc( V1Cfgs : 'X' )
017900120727     C     KTAB2         chain     TABEL
018000120727if  3C                   if        Not %found(TABEL00F)  or
018100120727     C                             TBLflg <> *blank
018200120727     C                   seton                                        2842
018300120727     C                   eval      $Msg = Err(10)
018400120727e   3C                   endif
018500120727e   2C                   endif
018600120712      *
018700120712     C                   Z-ADD     V1CFGS        KFGS
018800120712     C                   Z-ADD     WDTSTA        KDST
018900120712      * verifica se esistono righe da stampare
019000120712     c                   eval      wOk = *off
019100120712     C     KARB          setll     FNARBX1L
019200120712do  3c                   do        *hival
019300120712     C     KARB          reade     FNARBX1L
019400120712if  4c                   if        %eof(fnarbx1l)
019500120712     c                   leave
019600120712e   4c                   endif
019700120712if  4c                   if        arbtpl = 'R' or arbtpl = 'D'
019800120712     c                   iter
019900120712e   4c                   endif
020000120712     c                   eval      wOk = *on
020100120712     c                   leave
020200120712e   3c                   enddo
020300120712if  3C     wOk           IFEQ      *off
020400120712     C                   SETON                                        2841      data ristamp
020500120712     C                   MOVEL     ERR(8)        $MSG                           < data puliz.
020600120712     C                   leavesr
020700120712e   3C                   ENDIF
020800120712      *
020900120712e   2C                   ENDIF
021000120712      *
021100120712e   1C                   ENDIF
021200960117      *
021300960116     C     FINCT1        ENDSR
021400960116      *--------------------------------------------------------------*
021500960116      *   Operazioni di inizializzazione prima videata               *
021600960116      *--------------------------------------------------------------*
021700960116     C     INZ01         BEGSR
021800960116      *
021900120706     C                   clear                   LR79D01
022000960116     C                   Z-ADD     WFGS          V1CFGS
022100960116     C                   MOVEL     'N'           V1CRIS
022200120706     C                   MOVEL     'N'           V1CSNS
022300120706     C                   eval      *in03 = ( wrkdpd <> 'D' and
022400120706     C                                       wrkdpd <> 'X' )
022500120727     C                   if        *in03
022600120727     C                   eval      Kcod = 'WS'
022700120727     C                   eval      Kkey = %editc( Wfgs : 'X' )
022800120727     C     KTAB2         chain     TABEL
022900120727     C                   eval      *in03 = (%found(TABEL00F)  and
023000120727     C                                      TBLflg = *blank)
023100120727     C                   endif
023200120709     C                   eval      *in05 = ( SIMtpp <> *blank  and
023300120709     C                                       SIMtpp <> '2' )
023400120730     c                   if        $SbmJob = *on
023500120730     c                   eval      $SbmJob = *off
023600120730     c                   eval      *in28 = *on
023700120730     c                   eval      $MSG  = 'Il lavoro è stato sottomesso.'
023800120730     c                   endif
023900960116      *
024000960116     C                   ENDSR
024100120706      *--------------------------------------------------------------*
024200120706      *   Sottometto call a pgm stampa                               *
024300120706      *--------------------------------------------------------------*
024400120706     C     SBMP80        BEGSR
024500120706      *
024600120706     C                   CLEAR                   FNLR80DS
024700120706     C                   MOVE      V1CFGS        D80FGS
024800120706     C                   MOVE      V1CRIS        D80RIS
024900120709     C                   if        V1Cris = 'S'
025000120706     C                   MOVE      WDTSTA        D80DST
025100120706     C                   eval      D80sns = V1Csns
025200120709     C                   endif
025300120706     C*
025400120706     c                   select
025500120706     C                   when      wrkdpd = 'D'
025600120706     C                   MOVEL     'S'           D80DPD
025700120706     C                   when      wrkdpd = 'X'
025800120706     C                   MOVEL     'X'           D80DPD
025900120706     C                   other
026000120706     C                   MOVEL     'N'           D80DPD
026100120706     C                   endsl
026200120706     C*
026300120706     C                   MOVEL     FNLR80DS      KPJBU
026400120706     C                   MOVEL     'LR80'        KCOAZ
026500120730     C                   if        knmus = '1111111111'
026600120730     c                   call      'FNLR80R'
026700120730     c                   parm                    KPJBA
026800120730     c                   else
026900120706     C                   CALL      'BCH10'
027000120706     C                   PARM                    KPJBA
027100120730     c                   eval      $SbmJob = *on
027200120730     c                   endif
027300120706      *
027400120706     C                   ENDSR
027500960116      *--------------------------------------------------------------*
027600960116      *   Operazioni iniziali                                        *
027700960116      *--------------------------------------------------------------*
027800960116     C     *INZSR        BEGSR
027900960116      *
028000960116     C     *ENTRY        PLIST
028100960116     C                   PARM                    KPJBA
028200991130     C* REPERISCO FLAG DI STAMPA PER DPD.
028300991130     C                   MOVEL     KPJBU         WRKDPD            1
028400021029      * se il valore è a 'X' lista le bolle con merce da distruggere
028500021029     c     wrkdpd        comp      'X'                                    99
028600960116      *  Call a XPARUT
028700960116     C                   Z-ADD     1             CODUT
028800960116     C                   CALL      'X§PARUT'
028900960116     C                   PARM                    UT§DSE
029000960116     C     CODUT         CABEQ     -1            FINE
029100960116     C                   MOVEL     RAGUT         RSUT             20
029200960116     C                   MOVEL     REC80         CNCR80
029300120709     C                   eval      VIDpgm = SDSpgm
029400960116      *----------------------------------------------*
029500960116      *  Definizione chiavi di accesso
029600960116     C     KTAB1         KLIST
029700960116     C                   KFLD                    KKUT
029800960116     C                   KFLD                    KCOD
029900960116     C     KTAB2         KLIST
030000960116     C                   KFLD                    KKUT
030100960116     C                   KFLD                    KCOD
030200960116     C                   KFLD                    KKEY
030300960116     C     KARB          KLIST
030400960116     C                   KFLD                    KFGS
030500960116     C                   KFLD                    KDST
030600960116      *----------------------------------------------*
030700960116      *  Definizione variabili
030800960116     C     *LIKE         DEFINE    TBLKUT        KKUT
030900960116     C     *LIKE         DEFINE    TBLCOD        KCOD
031000960116     C     *LIKE         DEFINE    TBLKEY        KKEY
031100960116     C     *LIKE         DEFINE    ARBFGS        WFGS
031200960116     C     *LIKE         DEFINE    ARBFGS        KFGS
031300960116     C     *LIKE         DEFINE    ARBDST        KDST
031400120709      *----------------------------------------------*
031500120709      *  Reperisco filiale in gestione
031600120709     C                   Z-ADD     0             WFGS
031700120709     C     SIMTPP        IFEQ      '2'
031800120709     C     SIMTPP        OREQ      *BLANKS
031900120709     C                   MOVE      SIMPOU        WFGS
032000120709     C                   ELSE
032100120709     C                   MOVEL     SIMFEL        WFGS
032200120709     C                   END
032300960116      *----------------------------------------------*
032400960116      *  Inizializzazione variabili
032500960116     C                   Z-ADD     0             £1
032600020723     C*                  MOVEA     *BLANKS       FIL
032700960116     C                   Z-ADD     1             KKUT
032800960116     C                   MOVEL     '1'           WTPVID            1
032900960116     C                   MOVEL     'N'           WFINE             1
033000960117      *  Calcolo la data del giorno
033100960117     C                   TIME                    WHHDAT           14 0
033200960117     C                   CLEAR                   WLBDA8
033300960117     C                   Z-ADD     WHHDAT        G02DAT
033400960117     C                   CALL      'XSRDA8'
033500960117     C                   PARM                    WLBDA8
033600960117     C                   Z-ADD     G02INV        WOGGI             8 0
033700960117     C                   Z-ADD     G02TGI        WGIOR             5 0
033800960116      *----------------------------------------------*
033900960116      *  Caricamento delle tabelle
034000020723     c                   clear                   trul06ds
034100020723     c                   eval      d06cod = '£1'
034200020723     c                   movel     simfel        d06key
034300020723     c                   movel     trul06ds      kpjbu
034400020723     c                   call      'TRUL06R'
034500020723     c                   parm                    kpjba
034600020723     c                   movel     kpjbu         trul06ds
034700020723     c                   movel     lin           £1
034800020723
034900020723     c                   clear                   wc                3 0
035000020723 b1  c                   do        30            wa                3 0
035100020723     c                   if        £1(wa) = *zeros
035200020723     c                   leave
035300020723     c                   endif
035400020723     c                   clear                   kpjbu
035500020723     c                   clear                   trul06ds
035600020723     c                   eval      d06cod = '£6'
035700020723     c                   movel     £1(wa)        d06key
035800020723     c                   eval      d06esc = 'S'
035900020723     c                   movel     trul06ds      kpjbu
036000020723     c                   call      'TRUL06R'
036100020723     c                   parm                    kpjba
036200020723     c                   movel     kpjbu         trul06ds
036300020723 b2  c                   do        30            wb                3 0
036400020723     c                   if        lin(wb) = *zeros
036500020723     c                   leave
036600020723     c                   endif
036700020723     c                   add       1             wc
036800020723     c                   z-add     lin(wb)       l£6(wc)
036900020723 e2  c                   enddo
037000020723 e1  c                   enddo
037100960117      *----------------------------------------------*
037200960117      *  Carico gg. per controllo data pulizia
037300970718     C                   MOVEL     '5A'          KCOD
037400960117     C                   MOVEL     *BLANKS       KKEY
037500970718     C                   MOVEL     '2'           KKEY
037600960117     C     KTAB2         CHAIN     TABEL00F                           30
037700960117     C     *IN30         IFEQ      '0'
037800960117     C     TBLFLG        ANDEQ     ' '
037900970718     C                   MOVEL     TBLUNI        DS5A2
038000960117     C                   END
038100960117      *  Calcolo data limite pulizia
038200970718     C     WGIOR         SUB       §5ASTB        WGGPUL            5 0
038300960117     C                   MOVE      *ZEROS        GI8DAT
038400960117     C                   MOVE      *ZEROS        GI8INV
038500960117     C                   Z-ADD     WGGPUL        GI8TGI
038600960117     C                   CALL      'XSRGI8'
038700960117     C                   PARM                    WGIDAT
038800960117     C                   MOVE      GI8INV        WDTPUL            8 0
038900960116      *----------------------------------------------*
039000960116     C                   ENDSR
039100960116**
039200960116Filiale non gestita su questo AS                                      01
039300960116Filiale di consegna gestita da un'altra filiale                       02
039400960116Data obbligatoria in caso di ristampa                                 03
039500120727Data da indicare solo se richiesta ristampa                           04
039600960116Data stampa superiore alla data del giorno                            05
039700960116Data formalmente errata                                               06
039800960117Data inferiore alla data di pulizia dell'archivio di stampa           07
039900960116Non ci sono bolle da stampare per la data richiesta                   08
040000120727Selezione da indicare solo se richiesta ristampa                      09
040100120727Filiale in gestione non abilitata alla scannerizzazione: lasciare "N" 10
