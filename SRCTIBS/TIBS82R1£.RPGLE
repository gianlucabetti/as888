000100060102     H DECEDIT('0,') DATEDIT(*dMy.)
000200050104      * TIBS82R1 *---------------------------------------------------*
000300050104      *  - MODIFICA PUNTO OPERATIVO: sistemo file FISGN00F
000400940915      *--------------------------------------------------------------*
000500090116     Ffisgn12l  iF   E           K DISK    infds(sgn01)
000600090119     Ffisgn01l  uF   E           K DISK    prefix(O_)
000700090119     f                                     rename(fisgn000:fisgn001)
000800060102     Fazorg01l  iF   E           K DISK
000900090116     Ffisgn00f  uF   E             DISK    rename(fisgn000:fisgnfis)
001000050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
001100960302     D*
001200050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001300090116     D sgn01           DS
001400090116     D  sgnNRR               397    400B 0
001500090116
001600040917     C**********************************************************************
001700040917     C     *ENTRY        PLIST
001800050103     C                   PARM                    parpo             3 0
001900050103     C                   PARM                    partpo            3 0
002000050103     C                   PARM                    partpn            3 0
002100050103     c
002200050103     c                   eval      comman=ovrp(1)
002300050103     C                   CALL      'QCMDEXC'
002400050103     C                   PARM                    COMMAN           80
002500050103     C                   PARM      80            LUNG             15 5
002600050103     c
002700050103     c                   open      qsysprt
002800060102     c
002900060102     c     parpo         chain     azorg01l
003000060102     c                   if        %found(azorg01l)
003100060102     c                   movel     orgdes        despo            25
003200060102     c                   else
003300060102     c                   clear                   despo
003400060102     c                   endif
003500090119     c     partpo        chain     azorg01l
003600090119     c                   if        %found(azorg01l)
003700090119     c                   movel     orgdes        destpo           15
003800090119     c                   else
003900090119     c                   clear                   destpo
004000090119     c                   endif
004100090119     c     partpn        chain     azorg01l
004200090119     c                   if        %found(azorg01l)
004300090119     c                   movel     orgdes        destpn           15
004400090119     c                   else
004500090119     c                   clear                   destpn
004600090119     c                   endif
004700090119
004800090119     c                   except    testata
004900090119     c
005000090119     c*-----------------------------------------------------------------
005100090119     C     KSGN01        KLIST
005200090119     C                   KFLD                    partpn
005300090119     C                   KFLD                    SGNWHO
005400090119     c*-----------------------------------------------------------------
005500050103     c
005600050104     c* Leggo tutti i record di fisgn
005700050104     c     partpo        setll     fisgn12l
005800050104     c     partpo        reade     fisgn12l
005900050104    1c                   dow       not %eof(fisgn12l)
006000050103     c                   clear                   welabora          1
006100090119     c
006200090119    2c                   if        parpo=partpo    and sgnatb=' '
006300050104    3c                   if        sgntnp=parpo
006400050103     c                   eval      welabora='S'
006500050103    3c                   endif
006600050103     c
006700050103   X2c                   else
006800090119     c* altrimenti elaboro solo la linea  per il vecchio terminal
006900090119    3c                   if        sgnlnp=parpo and sgntnp=partpo
007000090119     c                             and sgnatb=' '
007100050103     c                   eval      welabora='S'
007200050103    3c                   endif
007300050103    2c                   endif
007400050103     c
007500050103    2c                   if        welabora='S'
007600090119     C* VERIFICO CHE NON SIA PRESENTE UN RECORD CON LO STESSO CHI SONO
007700090119     c                   clear                   recordok          1
007800090119     C     KSGN01        CHAIN     FISGN01L
007900090119     c* se trovasto verifico che non sia lo stesso
008000090119     c*  in questo caso vado oltre e non duplico
008100090119    3c                   if        %found(fisgn01l)
008200090119     c                   select
008300090119    4c                   when      O_sgnatb='*'
008400090119     c                   delete    fisgn001
008500090119     c                   eval      recordok='S'
008600090119     c
008700090119    4c                   when      O_sgnatb='A'
008800090119     c                   except    stampaerr
008900090119     c                   add       1             contaerr          7 0
009000090119     c                   unlock    fisgn01l
009100090119   x4c                   other
009200090119     c
009300090119    5c                   if        o_sgnlnp=sgnlnp and
009400090119     c                             o_sgnnsp=sgnnsp and
009500090119     c                             o_sgnnrs=sgnnrs and
009600090119     c                             o_sgnaas=sgnaas
009700090119     c                   add       1             contaesi          7 0
009800090119     c* lo riscrivo lo stesso
009900090119     c                   delete    fisgn001
010000090119     c                   eval      recordok='S'
010100090119     c
010200090119   x5c                   else
010300090119     c                   except    stampaerr
010400090119     c                   add       1             contaerr          7 0
010500090119    5c                   endif
010600090119    4c                   endsl
010700090119     c
010800090119   x3c                   else
010900090119     c                   eval      recordok='S'
011000090119    3c                   endif
011100090119     c
011200090119    3c                   if        recordok='S'
011300090116     c     sgnnrr        chain     fisgn00f
011400090119    4c                   if        %found
011500050104     c* aggiorno campo sgntnp
011600050104     c                   eval      sgntnp=partpn
011700090116     c                   clear                   sgnt6a
011800090116     c                   clear                   sgnt6b
011900090116     c                   clear                   sgnt6c
012000090116     c                   clear                   sgnt6d
012100090116     c                   clear                   sgnt6e
012200090116     c                   clear                   sgnt6f
012300090116     c
012400090116     c                   eval      sgndatora = %char(%timestamp:*iso0)
012500090116     c                   update    fisgnfis
012600050104     c                   add       1             contasgn          7 0
012700090119    4c                   endif
012800090119    3c                   endif
012900090116    2c                   endif
013000050104    c
013100050104     c     partpo        reade     fisgn12l
013200050103     C                   ENDDO
013300050103     c
013400050103     c* Stampo totale bolle aggiornate
013500050103     c                   except    totale
013600050103     c
013700050103     c                   seton                                        lr
013800090119     oQsysprt   e            testata        1  2
013900090119     o                                         +  0 'Filiale modificata'
014000090119     o                       parpo             +  1
014100090119     o                       despo             +  1
014200090119     o                                         +  2 'dal terminal '
014300090119     o                       partpo            +  1
014400090119     o                       destpo            +  1
014500090119     o                                         +  2 'al terminal '
014600090119     o                       partpn            +  1
014700090119     o                       destpn            +  1
014800090119     o                       *date             +  2 '  /  /    '
014900050103     oQsysprt   e            totale      2
015000050104     o                                         +  0 'Totale FISGN00F aggiornati'
015100090119     o                       CONTAsgn      2   +  1
015200090119     o                                         +  2 'Totale errori '
015300090119     o                       CONTAerr      2   +  1
015400090119     o                                         +  2 'di cui esistenti ma riscri'
015500090119     o                                         +  0 'tti'
015600090119     o                       CONTAesi      2   +  1
015700090119     oQsysprt   e            stampaerr   1
015800090119     o                                         +  0 'Errori CHI SONO già pr'
015900090119     o                                         +  0 'esente.Rec non aggiunto'
016000090119     o                       sgnATB            +  2
016100090119     o                       sgntnp            +  1
016200090119     o                       sgnwho            +  1
016300090119     o                       sgnaas            +  1
016400090119     o                       sgnlnp            +  1
016500090119     o                       sgnnrs        z   +  1
016600090119     o                       sgnnsp        z   +  1
016700090119     o                                         +  1 'NSC'
016800090119     o                       sgnfls            +  1
016900090119     o                       sgnlna            +  1
017000090119     o                       sgnncd        z   +  1
017100050103** ovrp
017200050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
