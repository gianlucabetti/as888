000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200981012     F* tibs03r * -----------------------------------------*
000300980727     F*          TRASMISSIONE TABELLE
000400000000     F*----------------------------------------------------*
000500981012     Ftntbe01l  UF   E           K DISK
000600981012     Ftntbe00t  uF a E           K DISK    USROPN
000700981012     F                                     RENAME(tntbe000:tntbet)
000800981012     D*----------------
000900981012     D* Ds
001000981012     D*----------------
001100981012     D wlbda8          DS                  INZ                                  *ds "XSRDA8"
001200981012     D  g08dat                        8  0                                       -data g/m/aa
001300981012     D  g08inv                        8  0                                       -data aa/m/g
001400981012     D  g08err                        1                                          -errore
001500981012     D  g08tgi                        5  0                                       -n°giorni data
001600000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001700000000     D KPJBA         E DS
001800020827     D DSCMZ         E DS
001900020827     D bs55ds        E DS                  EXTNAME(tibs55ds)                    *reper.librerie job
002000020902     D bs56ds        E DS                  EXTNAME(tibs56ds)                    *reper.librerie job
002100020902     d  fil                    7    756  0 DIM(250)                             *Terminal
002200981012      *
002300981012     D*----------------
002400981012     D* Schiere
002500020902     D*----------------
002600981012     d TBE             s              7  0 dim(250)                             *rec.tab x filiale
002700020827     D cm1             S              1    DIM(48) CTDATA PERRCD(48)            *comandi di tntbe00t
002800020827     D cm2             S              1    DIM(48) CTDATA PERRCD(48)            *comandi di tntbe00t
002900020827     D cm3             S              1    DIM(48) CTDATA PERRCD(48)            *comandi di tntbe00t
003000020827     D cm4             S              1    DIM(48) CTDATA PERRCD(48)            *comandi di tntbe00t
003100981012     D*----------------
003200981012     D*
003300981012     D*----------------
003400981012     d rec             s              7  0                                      *nrr in membro 000
003500981012     d x               s              3  0                                      * indice schiera
003600981012     d e               s              3  0                                      * indice schiera
003700981012     d com03           s              3  0                                      * comodo 3 numerico
003800020827     d var7            s              7                                         * comodo Membro
003900981012     D wn14            S             14  0                                      *numerico 14
004000981012     D wn8             S              8  0                                      *numerico 8
004100981012     D datcor          S              8  0                                      *d.corrente aa/m/g
004200020902     D lenght          S             15  5 INZ(48)                              *Lunghezza cmd CLP
004300981012     D comman          S             80                                         *valore cmd CLP
004400021007     D*------------
004500021007     D TRUL26DS      E DS                                                       *caricamento P.O.
004600021007     D  SKPO                  11    760  0 dim(250)
004700021007     D  SKPOT                761   1510  0 dim(250)
004800021007     D L1PO            S              3  0 DIM(250)
004900021007     D L1POT           S              3  0 DIM(250)
005000971021     F*----------------------------------------------------*
005100971021      * INDICATORI
005200981012     C*--------------------------------------------------------------------------------------------*
005300981012     C* Main lines
005400981012     C*--------------------------------------------------------------------------------------------*
005500981012     C*
005600981012     C* legge il file fisico e scrive il membro di trasmissione
005700981012     C                   EXSR      letscr
005800981012     C*
005900981012     C* trasmette il membro di trasmissione sia lo 000  che quello relativo alle singole filiali
006000021007     c                   if        rec >0
006100981012     C                   Z-ADD     1             e
006200981012do  1C                   DOW       fil(e) > *ZEROS
006300020902     C                   CLEAR                   BS55DS
006400020902     C                   z-add     046           I50PPO
006500020902     C                   z-add     fil(e)        I50APO
006600020902     C                   CALL      'TIBS55R'
006700020902     C                   PARM                    BS55DS
006800020902      *
006900020827     c                   movel     'M000000'     var7
007000981012     C                   EXSR      trasmi
007100981012     C                   ADD       1             e
007200981012e   1C                   ENDDO                                                  *fine filiali
007300981012     c* pulisco il membro 000
007400020827     c                   movel     '000'         wmbr
007500981012     c                   exsr      finsr
007600021007     c                   endif
007700981012     C* trasmetto solo alle filiali che hanno delle tabelle particolari
007800981012     C                   Z-ADD     1             e
007900981012do  1c                   dow       e<=250
008000981012if  2C                   if        tbe(e) > *ZEROS
008100020827     c                   movel     'M000   '     var7
008200020827     c                   move      fil(e)        var7
008300981012     c                   z-add     tbe(e)        rec
008400020902      *
008500020902     C                   CLEAR                   BS55DS
008600020902     C                   z-add     046           I50PPO
008700020902     C                   z-add     fil(e)        I50APO
008800020902     C                   CALL      'TIBS55R'
008900020902     C                   PARM                    BS55DS
009000020902      *
009100981012     C                   EXSR      trasmi
009200981012     c                   movel     fil(e)        wmbr
009300981012     c                   exsr      finsr
009400981012e   2c                   endif
009500981012     C                   ADD       1             e
009600981012e   1C                   ENDDO                                                  *fine filiali
009700981012     c*
009800981012     C                   SETON                                        lr
009900981012     c*--------------------------------------------------------------------------------------------*
010000981012     C* finsr - pulizia membri di trasmissione
010100981012     c*--------------------------------------------------------------------------------------------*
010200981012     C     finsr         BEGSR
010300981012     C*
010400981012     C* pulisce il membro di trasmissione fndct00t
010500020827     c                   movea     wmbr          cm4(45)
010600981012     c                   clear                   comman
010700981012     C                   MOVEa(P)  cm4           comman
010800981012     C                   CALL      'QCMDEXC'
010900981012     C                   PARM                    comman
011000981012     C                   PARM                    lenght
011100981012     c*
011200981012     c                   endsr
011300981012     C*
011400981012     c*--------------------------------------------------------------------------------------------*
011500981012     C* letscr - legge il file fisico e scrive il membro di trasmissione
011600981012     c*--------------------------------------------------------------------------------------------*
011700981012     C     letscr        BEGSR
011800981012     c*
011900980727     c     *loval        setll     tntbe01l
012000980727     c                   read      tntbe01l                               30
012100980727    1c     *in30         doweq     *off
012200980727     c**
012300980727     c* Record da non trasmettere
012400980727    2c     tbeftt        ifeq      ' '
012500980727     c*        o gia' trasmesso
012600980727     c     tbeftt        oreq      'S'
012700980727     c     tbeftr        andne     ' '
012800980727     c** annullato
012900980727    3c     tbeatb        ifne      '  '
013000980727     c                   delete    tntbe000
013100980727    3c                   endif
013200980727    2c                   endif
013300980727     c**
013400980727     c* record da trasmettere
013500980727    2c     tbeftt        ifeq      'S'
013600980727     c     tbeftr        andeq     ' '
013700980727     c* apro membro da scrivere
013800980727     c                   exsr      openmbr
013900980727     c*
014000981012     c                   open      tntbe00t
014100981012     c                   write     tntbet
014200981012     c                   close     tntbe00t
014300981012     c*
014400020827     c                   if        wmbr = '000'
014500981012     c                   add       1             rec
014600981012     c                   else
014700981012     c                   movel     wmbr          com03
014800981012     c                   z-add     1             e
014900981012     c     com03         lookup    fil(e)                                 35
015000981012     c   35              add       1             tbe(e)
015100981012     c                   endif
015200981012     c*
015300980727     c                   movel     'T'           tbeftr
015400981012     c                   z-add     datcor        tbedtr
015500980727     c                   update    tntbe000
015600980727    2c                   endif
015700980727     c*
015800980727     c                   read      tntbe01l                               30
015900980727    1c                   enddo
016000971016     C*
016100981012     c                   endsr
016200971015     c*--------------------------------------------------------------------
016300980727     c*  carico as a cui trasmettere le tabelle
016400971015     c*--------------------------------------------------------------------
016500980727     C     carpo         BEGSR                                                  *
016600020902     C* CHIAMO TIBS56R IN MODALITA' "GRU"
016700020902     C                   Z-ADD     *ZEROS        C                 4 0
016800020902     C                   CLEAR                   BS56DS
016900020902     C                   MOVEL     *zeros        I56PPO
017000020902     C                   MOVEL     'GRU'         I56MOD
017100020902     C                   CALL      'TIBS56R'
017200020902     C                   PARM                    BS56DS
017300021007      *
017400021007     C                   CLEAR                   TRUL26DS
017500021007     C                   CLEAR                   SKPO
017600021007     C                   CLEAR                   SKPOT
017700021007     C                   EVAL      I26TLA = 'L'
017800021007     C                   EVAL      I26TPT = 'G'
017900021007     C                   CALL      'TRUL26R'
018000021007     C                   PARM                    TRUL26DS
018100021007     C                   MOVEA     SKPO          L1PO                            -tutti i P.O.
018200021007     C                   MOVEA     SKPOT         L1POT                           -tutti i P.O.
018300971016     c                   endsr
018400971016     c*--------------------------------------------------------------------
018500980727     c*  apro membro da trasmettere - per tutti membro 000
018600971016     c*--------------------------------------------------------------------
018700980727     C     openmbr       BEGSR                                                  *
018800980727     c*. vedo se aprire membro 000 o lo specifico della fil
018900980727     c     tbeflt        ifgt      0
019000021007     c                   z-add     1             XX                3 0
019100021007     c     tbeflt        lookup    L1PO(XX)                               32
019200021007     c                   move      L1POT(XX)     wmbr
019300980727     c                   else
019400980727     c                   movel     '000'         wmbr              3
019500980727     c                   endif
019600981012     C*
019700981012     C* controlla se esiste il membro tntbe
019800020827     c                   movea     wmbr          cm1(45)
019900980727     c                   movea     cm1           comman
020000981013     c                   call      'QCMDEXC'                            20
020100981012     c                   parm                    comman
020200981012     C                   PARM                    lenght
020300981012     C*
020400981012     C* inesistente, aggiunge un membro nuovo tntbe
020500981012if  1C                   IF        *in20
020600020827     c                   movea     wmbr          cm2(45)
020700981012     C                   MOVEa(P)  cm2           comman
020800981012     C                   CALL      'QCMDEXC'
020900981012     C                   PARM                    comman
021000981012     C                   PARM                    lenght
021100981012e   1C                   ENDIF
021200981012     C*
021300981012     C* in ogni caso, esegue la sovrapposizione sul membro tntbe
021400020827     c                   movea     wmbr          cm3(45)
021500981012     C                   MOVEa(P)  cm3           comman
021600981012     C                   CALL      'QCMDEXC'
021700981012     C                   PARM                    comman
021800981012     C                   PARM                    lenght
021900980727     c**
022000981012     C                   ENDSR
022100981012     c*--------------------------------------------------------------------------------------------*
022200981012     C* trasmi - trasmette il membro di trasmissione
022300981012     c*--------------------------------------------------------------------------------------------*
022400981012     C     trasmi        BEGSR
022500981012     c*
022600981012     C                   MOVEL     *blanks       cmzprr                          *prerequisito
022700981012     c*
022800981012     c                   movel     o50plg        cmzlbd                         *dalla libreria
022900020827     c                   movel     o50alg        cmzlba                         *alla  libreria
023000021003     c                   movel     o50Asi        cmzsyf                         *sist.informativo
023100021003     c****               movel     o50psi        cmzsyf                         *sist.informativo
023200981012     c*
023300981012     c
023400981012     C                   MOVEL     fil(e)        cmzdst                          *destinatario
023500981012     C                   MOVEL     'TNTBE00T'    cmzfld                          *dal file -trasm-
023600020827     C                   MOVEL(P)  var7          cmzmbd                          *dal membro
023700981012     C                   MOVEL     'TNTBE00R'    cmzfla                          *al file  -ricez-
023800020827     C                   MOVEL(P)  var7          cmzmba                          *al membro
023900981012     C                   MOVE      '0000'        cmzori                          *ora invio
024000981012     C                   MOVE      '0000000'     cmzdti                          *data invio
024100981012     C                   MOVEL     'T'           cmzurg                          *urgenza notte
024200981012     C                   MOVEL     'A'           cmzadd                          *record in aggiunta
024300981012     C                   MOVEL     'N'           cmzclr                          *no pulizia file
024400981012     C                   MOVEL     'S'           cmzftr                          *invio da transito
024500981012     C                   MOVEL     *blanks       cmzerr                          *flag errore('1')
024600981202     C                   MOVEL     '%S04'        cmzazn                          *azione ricezione
024700981012     C                   MOVE      rec           cmznrr                          *record inviati
024800981012     c                   movel     o50lcl        cmzlcl            1             *locale
024900020902     C                   move      *blanks       CMZ236          236             PRE-REQ.
025000020827     C                   CALL      'TIBS53C'
025100020902     C                   PARM                    o50lcl
025200981012     C                   PARM                    cmzdst
025300981012     C                   PARM                    cmzfld
025400981012     C                   PARM                    cmzmbd
025500981012     C                   PARM                    cmzlbd
025600981012     C                   PARM                    cmzfla
025700981012     C                   PARM                    cmzmba
025800981012     C                   PARM                    cmzlba
025900981012     C                   PARM                    cmzori
026000981012     C                   PARM                    cmzdti
026100981012     C                   PARM                    cmzurg
026200981012     C                   PARM                    cmzadd
026300981012     C                   PARM                    cmzclr
026400981012     C                   PARM                    cmzftr
026500981012     C                   PARM                    cmzerr
026600981012     C                   PARM                    cmzazn
026700981012     C                   PARM                    cmzsyf
026800981012     C                   PARM                    cmznrr
026900981012     C                   PARM                    cmzprr
027000020902     C                   PARM                    cmz236
027100981012     c*
027200981012     c                   endsr
027300981012     c*--------------------------------------------------------------------
027400981012     c*  *inzsr Routine iniziale
027500981012     c*--------------------------------------------------------------------
027600981012     C     *inzsr        BEGSR                                                  *
027700981012     c*.
027800981012     C     *ENTRY        PLIST                                                  *
027900981012     C                   PARM                    KPJBA                          *
028000981012     C                   Z-ADD     1             CODUT
028100981012     C                   CALL      'XPARUT'
028200981012     C                   PARM                    PARUT
028300981012     C                   MOVEL     RAGUT         RSUT             20
028400981012     c* carico tutti gli as a cui devo trasmettere le tabelle
028500981012     c                   exsr      carpo
028600020902      *
028700981012     C* calcola la data corrente
028800981012     C                   TIME                    wn14                           *ora(6) + data(8)
028900981012     C                   MOVE      wn14          wn8                            *data(8) in g/m/aa
029000981012     C                   Z-ADD     wn8           g08dat
029100981012     C                   Z-ADD     *zeros        g08inv
029200981012     C                   MOVEL     '0'           g08err
029300981012     C                   CALL      'XSRDA8'
029400981012     C                   PARM                    wlbda8
029500981012     C                   Z-ADD     g08inv        datcor                         *d.corrente aa/m/g
029600981012     c*
029700981012     c                   endsr
029800980727**         CM1
029900020827CHKOBJ OBJ(TNTBE00T) OBJTYPE(*FILE) MBR(M000000)
030000981012**         CM2
030100020827ADDPFM FILE(TNTBE00T)               MBR(M000000)
030200981012**         CM3
030300020827OVRDBF FILE(TNTBE00T)               MBR(M000000)
030400981012**         CM4
030500020827CLRPFM FILE(TNTBE00T)               MBR(M000000)
