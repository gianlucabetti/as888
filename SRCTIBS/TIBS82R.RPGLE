000100060102     H DECEDIT('0,') DATEDIT(*dmy.)
000200050103      * TIBS82R *----------------------------------------------------*
000300050103      *  - MODIFICA PUNTO OPERATIVO: sistemo bolle non partite BLP
000400940915      *--------------------------------------------------------------*
000500090115     Ffnblp45l  iF   E           K DISK    infds(blp01)
000600050103     FfnblT01l  UF   E           K DISK
000700060102     Fazorg01l  iF   E           K DISK
000800090115     Ffnblp00f  uF   E             DISK    rename(fnblp000:fnblpfis)
000900050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
001000960302     D*
001100050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001200090115     d
001300090115     D blp01           DS
001400090115     D  blpNRR               397    400B 0
001500090115
001600040917     C**********************************************************************
001700050103     c     kblp          KLIST
001800050103     c                   KFLD                    BLPAAS
001900050103     c                   KFLD                    BLPLNP
002000050103     c                   KFLD                    BLPNRS
002100050103     c                   KFLD                    BLPNSP
002200050103     C**********************************************************************
002300040917     C     *ENTRY        PLIST
002400050103     C                   PARM                    parpo             3 0
002500050103     C                   PARM                    partpo            3 0
002600050103     C                   PARM                    partpn            3 0
002700050103     c
002800050103     c                   eval      comman=ovrp(1)
002900050103     C                   CALL      'QCMDEXC'
003000050103     C                   PARM                    COMMAN           80
003100050103     C                   PARM      80            LUNG             15 5
003200050103     c
003300050103     c                   open      qsysprt
003400060102     c
003500060102     c     parpo         chain     azorg01l
003600060102     c
003700060102     c                   if        %found(azorg01l)
003800060102     c                   movel     orgdes        despo            25
003900060102     c                   else
004000060102     c                   clear                   despo
004100060102     c                   endif
004200050103     c
004300050103     c
004400050103     c* Leggo tutte le bolle non partite
004500050103     c     *loval        setll     fnblp45l
004600050103     c                   read      fnblp45l
004700050103    1c                   dow       not %eof(fnblp45l)
004800050103     c                   clear                   welabora          1
004900050103    2c                   if        parpo=partpo
005000050103    3c                   if        blptfp=parpo
005100050103     c                   eval      welabora='S'
005200050103    3c                   endif
005300050103     c
005400050103   X2c                   else
005500050103     c* altrimenti elaboro solo la linea
005600120203    3c                   if        blplnp=parpo  and blptfp<>partpn
005700050103     c                   eval      welabora='S'
005800050103    3c                   endif
005900050103    2c                   endif
006000050103     c
006100050103    2c                   if        welabora='S'
006200090115     c     blpnrr        chain     fnblp00f
006300090115   2ac                   if        %found
006400050103     c* controllo se borderizzata
006500050103    3c                   if        blpnfv<>0 and blpnfv<>99999
006600050103     c                   except    errnfv
006700050103     c                   clear                   blpnfv
006800050103     c                   clear                   blpdbr
006900050103     c                   exsr      aggblt
007000050103    3c                   endif
007100050103     c
007200050103     c                   eval      blptfp=partpn
007300090120     c                   eval      blpfle=partpn
007400090115     c                   update    fnblpfis
007500050103     c                   add       1             contablp          5 0
007600090115   2ac                   endif
007700090115    2c                   endif
007800090115
007900050103     c                   read      fnblp45l
008000050103     C                   ENDDO
008100050103     c
008200050103     c* Stampo totale bolle aggiornate
008300050103     c                   except    totale
008400050103     c
008500050103     c                   seton                                        lr
008600000828      *
008700951025     C**************************************************************************
008800951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
008900951025     C**************************************************************************
009000050103     C     aggblt        BEGSR
009100020213
009200050103     C     kblp          setll     fnblt01l
009300050103     C     kblp          reade     fnblt01l
009400050103     c                   dow       not %eof(fnblt01l)
009500050104     c                   if        bltdfv=0
009600050103     c                   clear                   bltnfv
009700050103     c                   update    fnblt000
009800050104     c                   endif
009900050103     C     kblp          reade     fnblt01l
010000050103     c                   enddo
010100050103     c
010200951025     C                   ENDSR
010300050103     oQsysprt   e            ERRNFV         1
010400050103     o                                         +  0 'BOLLA BORDERIZZATA NON '
010500050103     o                                         +  0 'PARTITA:'
010600050103     o                       BLPAAS            +  2
010700050103     o                       BLPLNP            +  1
010800050104     o                       BLPNRS        z   +  1
010900050103     o                       BLPNSP        2   +  1
011000050104     o                                         +  2 'su foglio'
011100050104     o                       BLPNFV        2   +  1
011200050104     o                                         +  2 'del'
011300060102     o                       BLPDBR            +  1 '    /  /  '
011400050103     oQsysprt   e            totale      2
011500060102     o                                         +  0 'Punto Operativo Modificato'
011600060102     o                       parpo             +  2
011700060102     o                       despo             +  2
011800060102     o                       *date             + 25 '  /  /    '
011900060102     oQsysprt   e            totale      2
012000050104     o                                         +  0 'Totale FNBLP00F aggiornate'
012100050103     o                       CONTABLP      2   +  2
012200050103** ovrp
012300050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
