000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300020828      * FNLR6R  *                                                    *
000400950524      *---------*                                                    *
000500020828      *        controlla su FNFVV e ENARB se una distinta esiste     *
000501020828      *
000502020828      * parametri di ingresso:
000503020828      *
000504020828      * tipo elaborazione (C/blanks/L)  obb.
000505020828      * tipo categoria distinta         obb. (4=solo 4/8=4+8)
000506020828      * filale in gestione distinta     obb.
000507020828      * numero distinta                 obb.
000508020828      * data   distinta                 fac.
000509020828      *
000510020828      * parametri di uscita:
000511020828      *
000512020828      * flag (N=non trovata in entrambi i files
000513020828      *       S=trovata in entrambi i files
000514020828      *       F=trovato solo foglio
000515020828      *       B=trovata solo bolla
000516020828      *
000700950524      *--------------------------------------------------------------*
001000020827     FFNFVV01L  iF   E           K DISK
001200950524     F*---------
001500020827     FFNARB70L  iF   E           K DISK
001501020828     D kpjba         E DS
001502020828     D FNLR6ds       E DS
001600020827     C     *ENTRY        PLIST
001700020827     C                   PARM                    KPJBA
001800020827     C                   MOVEL     KPJBU         FNLR6ds
001801020828     c                   z-add     d6fgs         arbifp
001802020828     c                   z-add     d6ndt         fvvnfv
001900020827     C*  Definisco chiavi di accesso
002000020827     C     KFVV          KLIST
002100020828     C                   KFLD                    fvvnpg
002200020828     C                   KFLD                    fvvnfv
002300020827     C                   KFLD                    d6fgs
002400020827     C     Karb          KLIST
002401020828     C                   KFLD                    arbifp
002600020827     C                   KFLD                    d6ndt
002601020828     C     Karb1         KLIST
002602020828     C                   KFLD                    arbifp
002603020828     C                   KFLD                    d6ndt
002604020828     C                   KFLD                    d6ddt
002605020828     c*
002606020828     c                   movel     'N'           d6flg
002700020828     c                   if        d6tla <> 'C'
002703020828     c* FOGLIO
002704020828     c* per cat. 8 controllo anche la 4
002705020828     c                   if        d6npg = 4 or d6npg = 8
002706020828     c                   z-add     4             fvvnpg
002707020828     c                   exsr      srfoglio
002708020828     c                   end
002709020828     c* se non trovata cat. 4 controlla cat. 8
002710020828     c                   if        d6npg = 8 and d6flg = 'N'
002711020828     c                   z-add     8             fvvnpg
002712020828     c                   exsr      srfoglio
002713020828     c                   end
002817020828     c* BOLLA
002818020828     c* se NON passata data distinta
002819020828     c                   if        d6ddt = 0
002821020828     c     karb          chain     fnarb70l
002822020828     c                   else
002823020828     c     karb1         chain     fnarb70l
002824020828     c                   end
002825020828     c                   if        %found(fnarb70l)
002826020828     c                   if        d6flg = 'F'
002827020828     c                   movel     'S'           d6flg
002828020828     c                   else
002829020828     c                   movel     'B'           d6flg
002830020828     c                   end
002831020828     c                   end
002832020828     c                   end
002849020828     c* fine
002850020828     c                   movel(p)  fnlr6ds       kpjbu
002851020828     c                   if        d6tla <> ' '
002900020828     c                   seton                                        lr
003000020828     c                   else
003100020828     c                   seton                                        rt
003200020828     c                   end
003201020828     c**********************************************************************
003202020828     c     srfoglio      begsr
003203020828     c**********************************************************************
003204020828     c* se NON passata data distinta
003205020828     c                   if        d6ddt = 0
003300020828     c     kfvv          chain     fnfvv01l
003400020828     c                   if        %found(fnfvv01l)
003500020828     c                   movel     'F'           d6flg
003600020828     c                   end
003700020828     c* passata data distinta
003800020828     c                   else
003900020828     c* FOGLIO
004000020828     c     kfvv          setll     fnfvv01l
004100020828     c                   do        *hival
004200020828     c     kfvv          reade     fnfvv01l
004300020828     c                   if        %eof(fnfvv01l)
004400020828     c                   leave
004500020828     c                   end
004600020828     c                   if        fvvdfv <> d6ddt
004700020828     c                   iter
004800020828     c                   end
004900020828     c                   movel     'F'           d6flg
005000020828     c                   leave
005100020828     c                   enddo
005200020828     c                   end
005300020828     c                   endsr
