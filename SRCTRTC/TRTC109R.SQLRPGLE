000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300050726      *     Visualizza QRY per tipo msg EDI
000400000000      *****************************************************************
000500060504     Frcvifcsum1iF   E           k disk    usropn rename(RCVDATI:SUMDATI)
000600060504     FRCViftsta1iF   E           k disk    usropn rename(RCVDATI:STADATI)
000700060504     Ftrtc109d  CF   E             WORKSTN
000800030116      *****************************************************************
000801060504     D CMD             S             78    DIM(2) CTDATA PERRCD(1)
000802060504      **
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001201060504     D EDIDSer       E DS
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900050704     D*------------------
002000050704     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002100050704     D*------------------
002200050704     D                 DS
002300050704     D  AGGAA                  1      4  0
002400050704     D  AGGMM                  5      6  0
002500050704     D  AGGAM                  1      6  0
002600050704     D  AGGGG                  7      8  0
002700050704     D  WDAC                   1      8  0
002800060504
002801060504     D  MSGDS          S            100
002802060504     D  LENGH          S             15  5
002900050510     D*-------------
003000050510     D* Reperimento nome PGM
003100050510     D STATUS         SDS           333
003200050510     D  DSPGM            *PROC
003300050510     D*-------------
003400050510     D digits          C                   CONST('0123456789')
003500030116      *****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800030120      *
003900030825     C* Imposta la UDATE  da TIME
004000030825     C                   TIME                    W0140            14 0
004100030825     C                   MOVE      W0140         UDATE8            8 0
004200030825     C*
004300030120      *  In base all'utente prende il P.O.
004400030120     C                   CLEAR                   Tibs36Ds
004500030120     c                   EVAL      I36ute = Knmus
004600030120     c                   EVAL      I36Tla = 'L'
004700030120     C                   CALL      'TIBS36R'
004800030120     C                   PARM                    Tibs36Ds
004900030827     c                   movel     KNMUS         ALFA3             3
005000030829     c                   setoff                                       0102
005100030829      * solo per EDP....:
005200030829     c                   if        ALFA3 ='EDP'
005300030829     c                   seton                                        01
005400030829     c                   end
005500030829      * per utenti di filiale:
005600030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005700030829     c                   seton                                        02
005800030829     c                   end
005801060504     C*
005802060504      * ?------------------------------------------------------------------ */
005803060504      * ?Call x reperire se deve mandare Msg ad un utente specifico
005804060504      * ?------------------------------------------------------------------ */
005805060504     c                   clear                   kpjbu
005806060504     c                   call      'TRTC95BR'
005807060504     c                   parm                    kpjba
005808060504     c                   movel     kpjbu         edidsER
005809060504      **
005900000000      *--------------------------------------------------------------*
006000000000      *             M A I N      L I N E
006100000000      *--------------------------------------------------------------*
006200000000     C                   Z-ADD     1             CODUT
006300000000     C                   CALL      'X§PARUT'
006400000000     C                   PARM                    UT§DSE
006500000000     C                   MOVEL     RAGUT         RSUT             20
006600000000     C                   MOVEL     RAGUT         RSUT2            20
006700000000     C                   MOVEL     REC80         CNCR80
006800050510     C                   MOVEL     DSPGM         NOMPGM
006900040819      *
007000040819     C     INIZIO        tag
007100050726     C                   move      *blank        D1Tip
007200050726     C                   Z-ADD     0             D1Num
007300050726     C                   Z-ADD     0             D1Lnp
007400050727     C                   Z-ADD     0             D1data
007500050727     C                   Z-ADD     0             D1ora
007600030825      *
007700000000      * Emetto il formato video
007800030116     C     Video1        TAG
007900020308     C                   SETOFF                                       90
008000050510     C                   EXFMT     d01
008100000000      *
008200000000     C   KC              GOTO      FINE                                         F3=USCITA
008300000000      *
008400000000      * Eseguo i  controlli sui campi immessi a video
008500000000     C                   EXSR      CONTR
008600040830      *
008700040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008800050726     C   99
008900040830     CorNkf              GOTO      Video1
009000030905      *
009100050726      *  Sflagga i records da re-inviare
009200050726     C                   EXSR      SFLAG
009300040819      *
009400050726      *  richiama il pgm per sottomettere l'invio
009500050726     c                   clear                   kpjbu
009600050726     c                   call      'TRTC104R'
009700040826     c                   parm                    kpjba
009800000000      *
009900050726     C                   GOTO      INIZIO
010000030116      *
010100000000     C     FINE          TAG
010200000000     C                   SETON                                        LR
010300000000      *--------------------------------------------------------------*
010400000000      * Routine per i controlli sui campi del formato video          *
010500000000      *--------------------------------------------------------------*
010600000000     C     CONTR         BEGSR
010700000000      *
010800030116     c                   movea     '0000000000'  *in(41)
010900040819     c                   movea     '0000000000'  *in(51)
011000040819      * --------------
011100050727      *  Data e Ora
011200050727     C                   Z-ADD     d1DATA        G02DAT
011300050727     C                   MOVEL     *BLANK        G02ERR
011400050727     C                   CALL      'XSRDA8'
011500050727     C                   PARM                    WLBDAT
011600050727     C     G02ERR        IFEQ      '1'
011700050727     c                   seton                                        4299
011800050727     C                   GOTO      endct1
011900050727     C                   Else
012000050727     C                   Z-ADD     G02INV        datAmg            8 0
012100050727     C                   Z-ADD     G02DAT        d1DATA
012200050727     C                   ENDIF
012300050727      *
012400050727     C     d1ORA         IFEQ      *zero
012500050727     c                   seton                                        4399
012600050727     C                   GOTO      endct1
012700050727     C                   ENDIF
012800050727      * --------------
012900050727     c                   if        d1tip ='STA'
013000060504     c                   open      RCViftsta1                           98
013100060504     c                   move      'RX'          RRelabora
013200060504     c                   z-add     d1lnp         RRlinea
013300060504     c                   z-add     d1num         RRnumero
013400050727     c                   clear                   trovato           1
013500060504     c     key_chk       setll     RCViftsta1
013600060504     c     key_chk       reade     RCViftsta1
013700060504     c                   if        %EoF(RCViftsta1)
013800050727     c                   seton                                        4199
013900050727     C                   GOTO      endct1
014000050727     c                   else
014100050727      * + msg
014200060504     c                   dow       not %EoF(RCViftsta1)
014300060504     c                   if        datAmg = RRDATELA  and
014400060504     c                             d1ora  = RRORAELA
014500050727     c                   move      'S'           trovato           1
014600050727     c                   leave
014700050727     c                   end
014800060504     c     key_chk       reade     RCViftsta1
014900050727     c                   endDO
015000050727     c                   end
015100050727     c                   if        trovato = *blank
015200050727     c                   seton                                        4199
015300050727     C                   GOTO      endct1
015400050727     c                   end
015500060504     c                   close     RCViftsta1                           98
015600050727     c                   end
015700050727      * --------------
015800050727     c                   if        d1tip ='SUM'
015900060504     c                   open      RCVifcsum1                           98
016000060504     c                   move      'RX'          RRelabora
016100060504     c                   z-add     d1lnp         RRlinea
016200060504     c                   z-add     d1num         RRnumero
016300050727     c                   clear                   trovato           1
016400060504     c     key_chk       setll     RCVifcsum1
016500060504     c     key_chk       reade     RCVifcsum1
016600060504     c                   if        %EoF(RCVifcsum1)
016700050727     c                   seton                                        4199
016800050727     C                   GOTO      endct1
016900050727     c                   else
017000050727      * + msg come Pracht
017100060504     c                   dow       not %EoF(RCVifcsum1)
017200060504     c                   if        datAmg = RRDATELA  and
017300060504     c                             d1ora  = RRORAELA
017400050727     c                   move      'S'           trovato           1
017500050727     c                   leave
017600050727     c                   end
017700060504     c     key_chk       reade     RCVifcsum1
017800050727     c                   endDO
017900050727     c                   end
018000050727     c                   if        trovato = *blank
018100050727     c                   seton                                        4199
018200050727     C                   GOTO      endct1
018300050727     c                   end
018400060504     c                   close     RCVifcsum1                           98
018500050727     c                   end
018600050727      * --------------
018700040819      *
018800050727     C     endct1        tag
018900050727     C                   ENDSR
019000050726      *--------------------------------------------------------------*
019100050726      * Esegue l'operazione di sflag per re-inviare i records        *
019200050726      *--------------------------------------------------------------*
019300050726     C     SFLAG         BEGSR
019400050726      *
019500050726      * --------------
019600050726     c                   if        d1tip ='STA'
019700050726     C/EXEC SQL
019800060504     C+ UPDATE RCVIFTSTA SET RRELABORA = '  ' WHERE RRNUMERO =
019900060504     C+ :d1num AND RRLINEA = :d1lnp AND RRELABORA ='RX' AND
020000060504     C+ RRDATELA = :datAmg AND RRORAELA = :d1ORA
020100050726     C/END-EXEC
020101060504     c                   movel(p)  'IFTSTA'      tipoF
020102060504      ***
020200050726     c                   end
020300050726      * --------------
020400050726     c                   if        d1tip ='SUM'
020500050726     C/EXEC SQL
020600060504     C+ UPDATE RCVIFCSUM SET RRELABORA = '  ' WHERE RRNUMERO =
020700060504     C+ :d1num AND RRLINEA = :d1lnp AND RRELABORA ='RX' AND
020800060504     C+ RRDATELA = :datAmg AND RRORAELA = :d1ORA
020900050726     C/END-EXEC
020901060504     c                   movel(p)  'IFCSUM'      tipoF
020902060504      ***
021000050726     c                   end
021100050726      * --------------
021101060504      * strcmtctl
021102060504     C                   clear                   MSGDS
021103060504     C                   MOVEL     CMD(1)        MSGDS
021104060504     C                   Z-ADD     100           LENGH
021105060504     C                   CALL      'QCMDEXC'
021106060504     C                   PARM                    MSGDS
021107060504     C                   PARM                    LENGH
021108060504      *
021109060504     c                   clear                   flgTest
021110060504     c                   movel(p)  §ERUSR        usrmsg
021111060504     c                   call      'TRTC100R'
021112060504     c                   parm                    flgTest           1
021113060504     c                   parm                    usrmsg           10
021114060504     c                   parm                    tipoF            10
021115060504      * endcmtctl
021116060504     C                   clear                   MSGDS
021117060504     C                   MOVEL     CMD(2)        MSGDS
021118060504     C                   Z-ADD     100           LENGH
021119060504     C                   CALL      'QCMDEXC'
021120060504     C                   PARM                    MSGDS
021121060504     C                   PARM                    LENGH
021200050726      *
021300050726     C                   ENDSR
021400030116      *--------------------------------------------------------------*
021500050704      *   x decodifiche iniziali
021600050704      *--------------------------------------------------------------*
021700050704     C     *InzSR        begSR
021800050704     C*
021900050726     c     key_Chk       klist
022000060504     c                   kfld                    RRelabora
022100060504     c                   kfld                    RRlinea
022200060504     c                   kfld                    RRnumero
022300050704     C*---
022400050704     C                   ENDSR
022500050704      *--------------------------------------------------------------*
022600060504** (CMD)
022700060504STRCMTCTL LCKLVL(*CHG) CMTSCOPE(*JOB)
022800060504ENDCMTCTL
