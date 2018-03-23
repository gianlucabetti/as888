000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300061227      * FIDG03R *                                                    *
000400950524      *---------*                                                    *
000500061227      *        controlla su FIDST e FNARB se una distinta esiste     *
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
000701061227     fazorg01l  if   e           k disk
001000070410     FFIDST09L  iF   E           K DISK
001200950524     F*---------
001500020827     FFNARB70L  iF   E           K DISK
001501061227     F*iarg01L  iF   E           K DISK    usropn
001502020828     D kpjba         E DS
001503061227     D Fidg03ds      E DS
001504061227     D og146         E DS
001600020827     C     *ENTRY        PLIST
001700020827     C                   PARM                    KPJBA
001800061227     C                   MOVEL     KPJBU         Fidg03ds
001801061227     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
001802061227     c* quindi controllo FIARG
001803061227     c* se data distinte automatiche < oggi p.o. partito
001804061227     C     d3fgs         CHAIN     AZORG01l
001805061227    1C                   IF        %found(azorg01l)
001807061227     C                   MOVEL     ORGde6        og146
001808061227     c                   if        §ogdda <>' '
001813061227     c                   move      §ogdda        com08             8 0
001814061227     c                   if        com08 = 20391231
001815061227     c*                  open      fiarg01l
001819061227     c                   end
001820061227     c                   end
001821061227     c                   end
001823061227     c                   z-add     d3fgs         arbifp
001824061227     c                   z-add     d3ndt         dstnfv
001900020827     C*  Definisco chiavi di accesso
002000061227     C     Kdst          KLIST
002001070410     C                   KFLD                    d3fgs
002200061227     C                   KFLD                    dstnpg
002202061227     C                   KFLD                    dstnfv
002400020827     C     Karb          KLIST
002401020828     C                   KFLD                    arbifp
002600061227     C                   KFLD                    d3ndt
002601020828     C     Karb1         KLIST
002602020828     C                   KFLD                    arbifp
002603061227     C                   KFLD                    d3ndt
002604061227     C                   KFLD                    d3ddt
002605020828     c*
002606061227     c                   movel     'N'           d3flg
002700061227     c                   if        d3tla <> 'C'
002703020828     c* FOGLIO
002704020828     c* per cat. 8 controllo anche la 4
002705061227     c                   if        d3npg = 4 or d3npg = 8
002706061227     c                   z-add     4             dstnpg
002707020828     c                   exsr      srfoglio
002708020828     c                   end
002709020828     c* se non trovata cat. 4 controlla cat. 8
002710061227     c                   if        d3npg = 8 and d3flg = 'N'
002711061227     c                   z-add     8             dstnpg
002712020828     c                   exsr      srfoglio
002713020828     c                   end
002817020828     c* BOLLA
002818020828     c* se NON passata data distinta
002819061227     c                   if        d3ddt = 0
002820061227     c*                  if        %open(fiarg01l)
002821061227     c*    karb          chain     fiarg01l
002822061227     c*                  else
002823020828     c     karb          chain     fnarb70l
002824061227     c*                  end
002825020828     c                   else
002826061227     c*                  if        %open(fiarg01l)
002827061227     c*    karb1         chain     fiarg01l
002829061227     c*                  else
002830061227     c     karb1         chain     fnarb70l
002831061227     c*                  end
002832020828     c                   end
002833020828     c                   if        %found(fnarb70l)
002834061227     c                   if        d3flg = 'F'
002835061227     c                   movel     'S'           d3flg
002836020828     c                   else
002837061227     c                   movel     'B'           d3flg
002838020828     c                   end
002839020828     c                   end
002840020828     c                   end
002849020828     c* fine
002850061227     c                   movel(p)  fidg03ds      kpjbu
002851061227     c                   if        d3tla <> ' '
002900020828     c                   seton                                        lr
003000020828     c                   else
003100020828     c                   seton                                        rt
003200020828     c                   end
003201020828     c**********************************************************************
003202020828     c     srfoglio      begsr
003203020828     c**********************************************************************
003204020828     c* se NON passata data distinta
003205061227     c                   if        d3ddt = 0
003300070410     c     kdst          chain     FIDST09l
003400070410     c                   if        %found(FIDST09l)
003500061227     c                   movel     'F'           d3flg
003600020828     c                   end
003700020828     c* passata data distinta
003800020828     c                   else
003900020828     c* FOGLIO
004000070410     c     kdst          setll     FIDST09l
004100020828     c                   do        *hival
004200070410     c     kdst          reade     FIDST09l
004300070410     c                   if        %eof(FIDST09l)
004400020828     c                   leave
004500020828     c                   end
004600061227     c                   if        dstdfv <> d3ddt
004700020828     c                   iter
004800020828     c                   end
004900061227     c                   movel     'F'           d3flg
005000020828     c                   leave
005100020828     c                   enddo
005200020828     c                   end
005300020828     c                   endsr
