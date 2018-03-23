000100060102     H DECEDIT('0,') DATEDIT(*dmy.)
000200050103      * TIBS82R *----------------------------------------------------*
000300050103      *  - MODIFICA PUNTO OPERATIVO: sistemo bolle non partite BLP
000400940915      *--------------------------------------------------------------*
000500050103     Ffnblp45l  UF   E           K DISK
000600050103     FfnblT01l  UF   E           K DISK
000700060102     Fazorg01l  iF   E           K DISK
000800050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000900960302     D*
001000050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001100040917     C**********************************************************************
001200050103     c     kblp          KLIST
001300050103     c                   KFLD                    BLPAAS
001400050103     c                   KFLD                    BLPLNP
001500050103     c                   KFLD                    BLPNRS
001600050103     c                   KFLD                    BLPNSP
001700050103     C**********************************************************************
001800040917     C     *ENTRY        PLIST
001900050103     C                   PARM                    parpo             3 0
002000050103     C                   PARM                    partpo            3 0
002100050103     C                   PARM                    partpn            3 0
002200050103     c
002300050103     c                   eval      comman=ovrp(1)
002400050103     C                   CALL      'QCMDEXC'
002500050103     C                   PARM                    COMMAN           80
002600050103     C                   PARM      80            LUNG             15 5
002700050103     c
002800050103     c                   open      qsysprt
002900060102     c
003000060102     c     parpo         chain     azorg01l
003100060102     c
003200060102     c                   if        %found(azorg01l)
003300060102     c                   movel     orgdes        despo            25
003400060102     c                   else
003500060102     c                   clear                   despo
003600060102     c                   endif
003700050103     c
003800050103     c
003900050103     c* Leggo tutte le bolle non partite
004000050103     c     *loval        setll     fnblp45l
004100050103     c                   read      fnblp45l
004200050103    1c                   dow       not %eof(fnblp45l)
004300050103     c                   clear                   welabora          1
004400050103    2c                   if        parpo=partpo
004500050103    3c                   if        blptfp=parpo
004600050103     c                   eval      welabora='S'
004700050103    3c                   endif
004800050103     c
004900050103   X2c                   else
005000050103     c* altrimenti elaboro solo la linea
005100050103    3c                   if        blplnp=parpo
005200050103     c                   eval      welabora='S'
005300050103    3c                   endif
005400050103    2c                   endif
005500050103     c
005600050103    2c                   if        welabora='S'
005700050103     c* controllo se borderizzata
005800050103    3c                   if        blpnfv<>0 and blpnfv<>99999
005900050103     c                   except    errnfv
006000050103     c                   clear                   blpnfv
006100050103     c                   clear                   blpdbr
006200050103     c                   exsr      aggblt
006300050103    3c                   endif
006400050103     c
006500050103     c                   eval      blptfp=partpn
006600050103     c                   update    fnblp000
006700050103     c                   add       1             contablp          5 0
006800050103    2c                   endif
006900050103     c                   read      fnblp45l
007000050103     C                   ENDDO
007100050103     c
007200050103     c* Stampo totale bolle aggiornate
007300050103     c                   except    totale
007400050103     c
007500050103     c                   seton                                        lr
007600000828      *
007700951025     C**************************************************************************
007800951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
007900951025     C**************************************************************************
008000050103     C     aggblt        BEGSR
008100020213
008200050103     C     kblp          setll     fnblt01l
008300050103     C     kblp          reade     fnblt01l
008400050103     c                   dow       not %eof(fnblt01l)
008500050104     c                   if        bltdfv=0
008600050103     c                   clear                   bltnfv
008700050103     c                   update    fnblt000
008800050104     c                   endif
008900050103     C     kblp          reade     fnblt01l
009000050103     c                   enddo
009100050103     c
009200951025     C                   ENDSR
009300050103     oQsysprt   e            ERRNFV         1
009400050103     o                                         +  0 'BOLLA BORDERIZZATA NON '
009500050103     o                                         +  0 'PARTITA:'
009600050103     o                       BLPAAS            +  2
009700050103     o                       BLPLNP            +  1
009800050104     o                       BLPNRS        z   +  1
009900050103     o                       BLPNSP        2   +  1
010000050104     o                                         +  2 'su foglio'
010100050104     o                       BLPNFV        2   +  1
010200050104     o                                         +  2 'del'
010300060102     o                       BLPDBR            +  1 '    /  /  '
010400050103     oQsysprt   e            totale      2
010500060102     o                                         +  0 'Punto Operativo Modificato'
010600060102     o                       parpo             +  2
010700060102     o                       despo             +  2
010800060102     o                       *date             + 25 '  /  /    '
010900060102     oQsysprt   e            totale      2
011000050104     o                                         +  0 'Totale FNBLP00F aggiornate'
011100050103     o                       CONTABLP      2   +  2
011200050103** ovrp
011300050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
