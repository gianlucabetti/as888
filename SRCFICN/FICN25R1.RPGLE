000100030512     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300020308     Ffifgt03l  uf   e           k disk
000400030207     Ffifpt01l  if   e           k disk
000500090902     Ffiapd01l  if   e           k disk
000600020305     Ftabel00f  if   e           k disk
000700030227      *****************************************************************
000800030206     D C8a             S              3    DIM(99)                              Codice Tariffa
000900090903     D C8ar            S              3    DIM(99)                              Codice Tariffa rife.
001000030227     dkpjba          e ds
001100020305     dds8a           e ds
001200030226      *
001300020304     dparam            ds
001400020308     D lancio                         1
001500020304     d codpadr                        7s 0
001600020308     D codpadr2                       7s 0
001700020304     d alladat                        8s 0
001800030206     D  tipserv                       1
001900030227     D  autconv                       1
002000030206
002100030207      *---------------------------------------------------------------*
002200030207      *  Main
002300030207      *---------------------------------------------------------------*
002400090902      *  Carica le tabelle 8A
002500030206     c                   Exsr      CarTAB_Coop
002600030206      * Ciclo per Cooperative
002700020304     c                   z-add     codpadr       kpdr
002800021203     c     kapd          setll     fiapd01l
002900030207      *
003000020308     c                   do        *hival
003100030206     c     atip          reade     fiapd01l                               99
003200020308     c   99              leave
003300020308     c                   if        apdpdr > codpadr2
003400020308     c                   leave
003500020308     c                   endif
003600090902     c                   exsr      fifgt
003700030207      *
003800020308     c                   enddo
003900090903     c                   movel     param         kpjbu
004000020304
004100030206     c                   seton                                        LR
004200030207     C*--------------------------------------------------------------------
004300090902      * Tabelle 8A
004400030207     C*--------------------------------------------------------------------
004500030207     c     CarTAB_Coop   begsr
004600030207      *
004700030207     C*  Codici Tariffe Coop
004800090902     C                   z-add     0             X1                3 0
004900030207     C                   movel     '8A'          COD
005000030207     C     KTab_02       SETLL     tabel00f
005100030207     C     KTab_02       reade     tabel00f
005200030207     C*
005300030207     C                   Dow       not %Eof(tabel00f)
005400030207     C     tblFLG        ifeq      ' '
005500030207     C                   movel     tblUNI        ds8A
005600030207     C*
005700030207      * solo quelle delle Cooperative
005800030207     C*  ed il Tipo servizio è quello richiesto dal video
005900090903     C     §8afac        ifeq      'C'
006000090903     C     §8aobb        andeq     'S'
006100090902     C                   add       1             X1
006200090902     C                   movel     tblKEY        C8a(X1)
006300090903     C                   movel     §8avocr       C8ar(X1)
006400030207     C                   end
006500030207     C                   end
006600030207      *
006700030207     C     KTab_02       reade     tabel00f
006800030207     C                   EndDO
006900030207     C*
007000030207     c                   endSR
007100030207     C*--------------------------------------------------------------------
007200030207      * Aggancia la Tariffa della Cooperativa alla data
007300030207     C*--------------------------------------------------------------------
007400030207     c     fifgt         begsr
007500030227      *
007600030207     c                   setoff                                           99
007700030207     c     kfgt          setll     fifgt03l
007800030207     c                   do        *hival
007900030207     c     kfgt          reade     fifgt03l                               99
008000030207     c   99              leave
008100030207      *
008200030207      * non considero le tariffe che risultano fuori periodo
008300030207     c                   if        fgtddt > alladat or  fgtdst < alladat
008400030207     c                   iter
008500030207     c                   end
008600090902      * eseguo serie di chain per tutte le voci che devono essere obbligatorie se anche una
008700090902      * sola manca non do la possibilità di stampare la tariffa
008800090902     c                   do        *hival        x1
008900090902     c                   if        c8a(x1) = *blank
009000090902     c                   leave
009100090902     c                   else
009200090903     c                   move      c8ar(x1)      ctar
009300090903     c     kfpt          chain     fifpt01l
009400090908     c                   if        %found(fifpt01l) and fptatb = *blank
009500090903     c                   move      c8a(x1)       ctar
009600090902     c     kfpt          chain     fifpt01l
009700090902     c                   if        not %found(fifpt01l)
009800090902     c                   eval      autconv = 'E'
009900090903     c                   leave
010000090902     c                   endif
010100090921     c                   else
010200090921     c     kfpt          chain     fifpt01l
010300090921     c                   if        %found(fifpt01l)
010400090921     c                   eval      autconv = 'F'
010500090921     c                   leave
010600090921     c                   endif
010700090902     c                   endif
010800090903     c                   endif
010900090902     c                   enddo
011000030227      *
011100090902     c                   enddo
011200030227      *
011300030207     c                   endsr
011400030207     C*--------------------------------------------------------------------
011500030207     c     *inzsr        begsr
011600030207     C*--------------------------------------------------------------------
011700020304     c     *entry        plist
011800020304     c                   parm                    kpjba
011900020304     c                   movel     kpjbu         param
012000020304
012100030206     C     ktab_02       klist
012200030206     C                   kfld                    codut             1 0
012300030206     C                   kfld                    cod
012400030207      *
012500030206     c     kapd          klist
012600030206     c                   kfld                    atip
012700030206     c                   kfld                    kpdr
012800030207      *
012900030207      *
013000020304     c     kfgt          klist
013100030207     c                   kfld                    apdpdr
013200020304     c                   kfld                    ksml
013300020304     c                   kfld                    ktsr
013400090902     c                   movel     'X'           ktsr
013500030207      *------
013600020304     c     kfpt          klist
013700020304     c                   kfld                    fgtpdr
013800020304     c                   kfld                    fgtsml
013900020304     c                   kfld                    fgttsr
014000020304     c                   kfld                    fgtctr
014100040817     c                   kfld                    fgtprg
014200090902     c                   kfld                    CTar              3
014300090902      *
014400030207     c                   movel     'C'           atip              1
014500030206     c                   z-add     1             codut
014600030207     c                   z-add     codpadr       kpdr
014700030206     C*
014800020313     c     *like         define    fgtrap        krap
014900020313     c     *like         define    fgtrct        krct
015000020313     C     *LIKE         DEFINE    TBLCOD        COD
015100020305     C     *LIKE         DEFINE    TBLKEY        KEY
015200020305     C     *LIKE         DEFINE    fgtpdr        kpdr
015300020305     C     *LIKE         DEFINE    fgtsml        Ksml
015400020305     C     *LIKE         DEFINE    fgttsr        Ktsr
015500040817     c     *like         define    fgtprg        Kprg
015600020305
015700020218     C                   time                    w0120            14 0
015800020218     C                   move      w0120         wdat              8 0
015900030203     c
016000011026     c                   endsr
