000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200020529      *--------------------------------------------------------------*
000300020529      * FICNB10R *                                                   *
000400020529      *         - STAMPA ARCHIVIAZIONE SU OTTICO                     *
000500020529      *         - VALORIZZAZIONE GIORNALIERA AUTOTRASPORTATORI       *
000600020529      *--------------------------------------------------------------*
000700020529     Ffiftt07L  IF   E           K DISK
000800931102     D KPJBA         E DS
000900000623      *----------------------------------------------------------------
001000020529     D ficnb6ds      E DS
001100020529     D ficn58ds      E DS
001200020506     D dtaiso          S               d
001300020506     D dtaeur          S               d   datfmt(*eur)
001400931102      *--------------------------------------------------------------*
001500931102      *  CICLO PRINCIPALE:                                           *
001600931102      *--------------------------------------------------------------*
001700020529     C*  Loop di letture per società fornitore fattura padroncino data val.
001800020529     c* solo i record con tipo prestazione = ' '
001900020529     C     f58soc        setll     fiftt07L
002000020529     c                   do        *hival
002100020529     c     f58soc        reade     fiftt07l
002200020529     c                   if        %eof(fiftt07l)
002300020529     c                   leave
002400020529     c                   end
002500020529     c* controllo data fattura
002600020529     c                   if        fttdft < f58dti
002700020529     c                   z-add     f58dti        fttdft
002800020529     c     kftt          setll     fiftt07l
002900020529     c                   iter
003000020529     c                   end
003100021127     c                   if        fttdft > f58dtf
003200020529     c                   z-add     99999999      fttdft
003300020529     c     kftt          setgt     fiftt07l
003400020529     c                   iter
003500020529     c                   end
003600020529     c* chiamo pgm di valorizzazione giornaliera
003700020529     c                   clear                   ficnb6ds
003800020529     C                   MOVEl     fttpdr        DD0PDR
003900020529     C                   MOVEL     fttddc        DD0DDT
004000020529     C                   MOVEL     ficnb6ds      KPJBU
004100020529     c                   call      'FICNB6R'
004200020529     c                   parm                    kpjba
004300020529     c                   enddo
004400020529     c* chiamo pgm di valorizzazione giornaliera per chiudere file
004500020529     c                   clear                   ficnb6ds
004600020529     C                   MOVEL     ficnb6ds      KPJBU
004700020529     c                   call      'FICNB6R'
004800020529     c                   parm                    kpjba
004900020531     C                   MOVEL     ficn58ds      KPJBU
005000020529     c*
005100020510     C                   SETON                                        LR
005200020515      *--------------------------------------------------------------*
005300020515      *  *INZSR: Operazioni iniziali                                 *
005400020515      *--------------------------------------------------------------*
005500020515     C     *INZSR        BEGSR
005600931104      *--------------------------------------------------------------*
005700931104     C     *ENTRY        PLIST
005800931104     C                   PARM                    KPJBA
005900020529     C                   MOVEL     KPJBU         ficn58ds
006000020529     C     KFTT          KLIST
006100020529     C                   KFLD                    f58soc
006200020529     C                   KFLD                    fttcdf
006300020529     C                   KFLD                    fttdft
006400931108     C                   ENDSR
