000100020507     Fkpprf01l  if   e           k disk
000200020507     Futent00f  uf a e           k disk
000300020507     Futent02l  if   e           k disk
000400020507     F                                     RENAME(utent000:utent002)
000500020507     Fazute00f  O    e             disk
000600020507     d                 DS
000700020507     D  dute6                  1      6
000800020507     D  dute3                  1      3
000900020507     D  dutenum                4      6
001000020507     D*
001100020507     c*
001200020507     c                   clear                   wcont             3 0
001300020507     c*
001400020507     C     *loval        setll     kpprf01l
001500020507     c                   do        *hival
001600020507     C                   read      kpprf01l
001700020507      * fine file
001800020507     c                   if        %eof(kpprf01l)
001900020507     c                   leave
002000020507     c                   endif
002100020507     c*
002200020507     c                   clear                   azute000
002300020507     c*
002400020507     c                   z-add     20391231      utedsc
002500020507     c                   movel     prfnmu        uteute
002600020507     c     uteute        chain     utent00f                           30
002700020507     c     *in30         ifeq      *off
002800020507     c                   z-add     utncod        utecdu
002900020507     c                   else
003000020507     c                   exsr      riccdu
003100020507     c                   z-add     wcont         utecdu
003200020507     c                   z-add     wcont         utncod
003300020507     c                   movel     uteute        utnnam
003400020507     c                   write     utent000
003500020507     c                   endif
003600020507     c                   movel     uteute        dute6
003700020507     c     dute3         ifeq      'FIL'
003800020507     C     dutenum       andgt     *zeros
003900020507     c                   move      dutenum       utefil
004000020507     c                   else
004100020507     c                   z-add     46            utefil
004200020507     c                   endif
004300020507     c     utefil        ifeq      46
004400020507     c                   movel     'AZ'          uteaut
004500020507     c                   else
004600020507     c                   movel     'PO'          uteaut
004700020507     c                   endif
004800020507     c                   write     azute000
004900020507     c                   enddo
005000020507     c                   seton                                        lr
005100020507     c**********************************************************************
005200020507     c     riccdu        begsr
005300020507     c     wcont         setgt     utent02l
005400020507     c                   do        *hival
005500020507     c                   read      utent02l
005600020507      * fine file
005700020507     c                   if        %eof(utent02l)
005800020507     c                   leave
005900020507     c                   endif
006000020507     c*
006100020507     c                   if        (utncod - 1) <> wcont
006200020507     c                   add       1             wcont
006300020507     c                   leave
006400020507     c                   endif
006500020507     c                   z-add     utncod        wcont
006600020507     c*
006700020507     c                   enddo
006800020507     c
006900020507     c                   endsr
