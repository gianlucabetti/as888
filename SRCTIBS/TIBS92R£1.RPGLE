000100060102     H DECEDIT('0,') DATEDIT(*dMy.)
000200050104      * TIBS82R1 *---------------------------------------------------*
000300050104      *  - MODIFICA PUNTO OPERATIVO: sistemo file FISGN00F
000400940915      *--------------------------------------------------------------*
000500090116     Ffisgn12l  iF   E           K DISK    infds(sgn01)
000600060102     Fazorg01l  iF   E           K DISK
000700090116     Ffisgn00f  uF   E             DISK    rename(fisgn000:fisgnfis)
000800050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000900960302     D*
001000050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001100090116     D sgn01           DS
001200090116     D  sgnNRR               397    400B 0
001300090116
001400040917     C**********************************************************************
001500040917     C     *ENTRY        PLIST
001600050103     C                   PARM                    parpo             3 0
001700050103     C                   PARM                    partpo            3 0
001800050103     C                   PARM                    partpn            3 0
001900050103     c
002000050103     c                   eval      comman=ovrp(1)
002100050103     C                   CALL      'QCMDEXC'
002200050103     C                   PARM                    COMMAN           80
002300050103     C                   PARM      80            LUNG             15 5
002400050103     c
002500050103     c                   open      qsysprt
002600060102     c
002700060102     c     parpo         chain     azorg01l
002800060102     c
002900060102     c                   if        %found(azorg01l)
003000060102     c                   movel     orgdes        despo            25
003100060102     c                   else
003200060102     c                   clear                   despo
003300060102     c                   endif
003400060102     c
003500050103     c
003600050103     c
003700050104     c* Leggo tutti i record di fisgn
003800050104     c     partpo        setll     fisgn12l
003900050104     c     partpo        reade     fisgn12l
004000050104    1c                   dow       not %eof(fisgn12l)
004100050103     c                   clear                   welabora          1
004200050103    2c                   if        parpo=partpo
004300050104    3c                   if        sgntnp=parpo
004400050103     c                   eval      welabora='S'
004500050103    3c                   endif
004600050103     c
004700050103   X2c                   else
004800050103     c* altrimenti elaboro solo la linea
004900050104    3c                   if        sgnlnp=parpo
005000050103     c                   eval      welabora='S'
005100050103    3c                   endif
005200050103    2c                   endif
005300050103     c
005400050103    2c                   if        welabora='S'
005500090116     c     sgnnrr        chain     fisgn00f
005600090116   2ac                   if        %found
005700050104     c* aggiorno campo sgntnp
005800050104     c                   eval      sgntnp=partpn
005900090116     c                   clear                   sgnt6a
006000090116     c                   clear                   sgnt6b
006100090116     c                   clear                   sgnt6c
006200090116     c                   clear                   sgnt6d
006300090116     c                   clear                   sgnt6e
006400090116     c                   clear                   sgnt6f
006500090116     c
006600090116     c                   eval      sgndatora = %char(%timestamp:*iso0)
006700090116     c                   update    fisgnfis
006800050104     c                   add       1             contasgn          7 0
006900050103    2c                   endif
007000090116    2c                   endif
007100050104    c
007200050104     c     partpo        reade     fisgn12l
007300050103     C                   ENDDO
007400050103     c
007500050103     c* Stampo totale bolle aggiornate
007600050103     c                   except    totale
007700050103     c
007800050103     c                   seton                                        lr
007900060102     oQsysprt   e            totale      2
008000060102     o                                         +  0 'Punto Operativo Modificato'
008100060102     o                       parpo             +  2
008200060102     o                       despo             +  2
008300060102     o                       *date             + 25 '  /  /    '
008400050103     oQsysprt   e            totale      2
008500050104     o                                         +  0 'Totale FISGN00F aggiornati'
008600050104     o                       CONTAsgn      2   +  2
008700050103** ovrp
008800050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
