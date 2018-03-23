000100030512     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300020308     Ffifgt03l  uf   e           k disk
000400030207     Ffifpt01l  if   e           k disk
000500090902     Ffiapd01l  if   e           k disk
000600020305     Ftabel00f  if   e           k disk
000700030227      *****************************************************************
000800030227      *  Riepilogo Indicatori utilizzati per la Stampa
000900030227      *  01 = (on) Overflow
001000030227      *  21 = Generico
001100030227      *  44 = Stampa x Tariffe Pacchi Poste
001200030227      *  99 = Generico
001300030227      *****************************************************************
001400030206     D C8a             S              3    DIM(99)                              Codice Tariffa
001500030227     dkpjba          e ds
001600020305     dds8a           e ds
001700030226      *
001800020304     dparam            ds
001900020308     D lancio                         1
002000020304     d codpadr                        7s 0
002100020308     D codpadr2                       7s 0
002200020304     d alladat                        8s 0
002300030206     D  tipserv                       1
002400030227     D  autconv                       1
002500030206
002600030207      *---------------------------------------------------------------*
002700030207      *  Main
002800030207      *---------------------------------------------------------------*
002900030206      *  Carica le tabelle 8A e 8F che servono per pilotare la stampa
003000030206     c                   Exsr      CarTAB_Coop
003100030206      * Ciclo per Cooperative
003200020304     c                   z-add     codpadr       kpdr
003300021203     c     kapd          setll     fiapd01l
003400030207      *
003500020308     c                   do        *hival
003600030206     c     atip          reade     fiapd01l                               99
003700020308     c   99              leave
003800020308     c                   if        apdpdr > codpadr2
003900020308     c                   leave
004000020308     c                   endif
004100020308     c                   exsr      elabora
004200030207      *
004300020308     c                   enddo
004400020304
004500030206     c                   seton                                        LR
004600030207     C*--------------------------------------------------------------------
004700090902      * Tabelle 8A
004800030207     C*--------------------------------------------------------------------
004900030207     c     CarTAB_Coop   begsr
005000030207      *
005100030207     C*  Codici Tariffe Coop
005200030207     C                   z-add     0             X8a               5 0
005300030207     C                   movel     '8A'          COD
005400030207     C     KTab_02       SETLL     tabel00f
005500030207     C     KTab_02       reade     tabel00f
005600030207     C*
005700030207     C                   Dow       not %Eof(tabel00f)
005800030207     C     tblFLG        ifeq      ' '
005900030207     C                   movel     tblUNI        ds8A
006000030207     C*
006100030207      * solo quelle delle Cooperative
006200030207     C*  ed il Tipo servizio è quello richiesto dal video
006300090902     C     §8aobb        ifeq      'S'
006400030207     C                   add       1             X8a
006500030207     C                   movel     tblKEY        C8a(X8a)
006600030207     C                   end
006700030207     C                   end
006800030207      *
006900030207     C     KTab_02       reade     tabel00f
007000030207     C                   EndDO
007100030207     C*
007200030207     c                   endSR
007300030207     C*--------------------------------------------------------------------
007400030207      * Aggancia la Tariffa della Cooperativa alla data
007500030207     C*--------------------------------------------------------------------
007600030207     c     fifgt         begsr
007700030207      *
007800030227     c                   clear                   tar_ok            1
007900030227      *
008000030207     c                   setoff                                           99
008100030207     c     kfgt          setll     fifgt03l
008200030207     c                   do        *hival
008300030207     c     kfgt          reade     fifgt03l                               99
008400030207     c   99              leave
008500030207      *
008600030207      * non considero le tariffe che risultano fuori periodo
008700030207     c                   if        fgtddt > alladat or  fgtdst < alladat
008800030207     c                   iter
008900030207     c                   end
009000030227      *
009100090902     c                   enddo
009200030227      *
009300030207     c                   endsr
009400030207     C*--------------------------------------------------------------------
009500030207     c     *inzsr        begsr
009600030207     C*--------------------------------------------------------------------
009700020304     c     *entry        plist
009800020304     c                   parm                    kpjba
009900020304     c                   movel     kpjbu         param
010000020304
010100030206     C     ktab_02       klist
010200030206     C                   kfld                    codut             1 0
010300030206     C                   kfld                    cod
010400030207      *
010500030206     C     ktab          klist
010600030206     C                   kfld                    codut
010700030206     C                   kfld                    cod
010800030206     C                   kfld                    key
010900030207      *
011000030206     c     kapd          klist
011100030206     c                   kfld                    atip
011200030206     c                   kfld                    kpdr
011300030207      *
011400030207      *
011500020304     c     kfgt          klist
011600030207     c                   kfld                    apdpdr
011700020304     c                   kfld                    ksml
011800020304     c                   kfld                    ktsr
011900030207      *
012000020313     c     kfgtx         klist
012100020313     c                   kfld                    krap
012200020313     c                   kfld                    fgtsml
012300020313     c                   kfld                    fgttsr
012400020313     c                   kfld                    krct
012500030207      *
012600030207      *------
012700020304     c     kfpt          klist
012800020304     c                   kfld                    fgtpdr
012900020304     c                   kfld                    fgtsml
013000020304     c                   kfld                    fgttsr
013100020304     c                   kfld                    fgtctr
013200040817     c                   kfld                    fgtprg
013300030207     c                   kfld                    CTar
013400090902      *
013500030207     c                   movel     'C'           atip              1
013600030206     c                   z-add     1             codut
013700030207     c                   z-add     codpadr       kpdr
013800030206     C*
013900020313     c     *like         define    fgtrap        krap
014000020313     c     *like         define    fgtrct        krct
014100020313     C     *LIKE         DEFINE    TBLCOD        COD
014200020305     C     *LIKE         DEFINE    TBLKEY        KEY
014300020305     C     *LIKE         DEFINE    fgtpdr        kpdr
014400020305     C     *LIKE         DEFINE    fgtsml        Ksml
014500020305     C     *LIKE         DEFINE    fgttsr        Ktsr
014600040817     c     *like         define    fgtprg        Kprg
014700020305
014800020218     C                   time                    w0120            14 0
014900020218     C                   move      w0120         wdat              8 0
015000020308      * data da impostare su data stampa e convalida se lancio = 'C'
015100020308     c                   move      wdat          dataeur
015200020308     c                   move      dataeur       dataiso
015300020308     c                   move      dataiso       dataoggi          8 0
015400020308      * data da elaborare
015500020304     c                   move      alladat       dataiso
015600020304     c                   move      dataiso       dataeur
015700020304     c                   move      dataeur       alladata          8 0
015800030203     c
015900011026     c                   endsr
