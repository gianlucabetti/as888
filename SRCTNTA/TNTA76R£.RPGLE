000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060517      * TNTA73R *----------------------------------------------------*
000300061127      *        Aggiorna codici fiscali su codici clienti
000400061127      *              collegati con la partita iva
000500940726      *--------------------------------------------------------------*
000600061127     fCnAco00f  uf   e           k disk
000700061127     fCnind02l  uf   e           k disk
000800061127     fcnind03l  if   e           k disk    rename(cnind000:cnind003)
000900061127     d
001000061127     d kpjba         e ds
001100061127     d Tibs34ds      e ds
001200061127     d dDatiute      e ds
001300061127     d Azuteds       e ds                  Extname(Azute00f)
001400061127     D Kut             S                   LIKE(indkut) inz(1)
001500061127     D Kcdf            S                   LIKE(indcdf) inz
001600061127     D aggcdf          S                   LIKE(indcdf)
001700061127     D Kiva            S                   LIKE(indiva)
001800021212      *--------------------------------------------------------------*     *****
001900021212     c     *ENTRY        PLIST
002000060523     c                   PARM                    kpjba
002100061127     c
002200061127     c* leggo la lf  dei codici fiscali con cdf<>*blanks
002300061127     c     kind03a       setgt     cnind03l
002400061127     c     kind03b       reade     cnind03l
002500061127     c
002600061127    1c                   dow       not %eof(cnind03l)
002700061127     c* Escludo la partiva iva : PRIVATO  o iso = $$
002800061127     c*                          vuota o = 00000000000
002900061127   1ac                   if        indiva<>*blanks and indiva<>'00000000000    '
003000061127     c                             and %subst(indiva:1:2)<>'$$'
003100061127     C     'PRIVATO'     SCAN      indiva                                 30
003200061127   1bc                   if        not *in30
003300061127     c* Mi salvo i dati necessari e verifico se ci sono altri
003400061127     c*  codici con la stessa p.iva
003500061127     c                   eval      kiva=indiva
003600061127     c                   eval      aggcdf=indcdf
003700061127     c
003800061127     c     kind02a       setll     cnind02l
003900061127     c     kind02a       reade     cnind02l
004000061127    2c                   dow       not %eof(cnind02l)
004100061127     c* Se non è il codice letto, aggiorno se vuota
004200061127    3c                   if        indcdf=*blanks
004300061127     c                   eval      indcdf=aggcdf
004400061127     c* chain su cnaco per sflaggare i dati
004500061127     c     kaco          chain     cnaco000
004600061127    4c                   if        %found(cnaco00f)
004700061127     c                   Clear                   acodtr
004800061127     c                   Clear                   acoftr
004900061127     c                   update    cnaco000
005000061127    4c                   endif
005100061127     c
005200061127     c                   update    cnind000
005300061127    3c                   endif
005400061127     c
005500061127     c     kind02a       reade     cnind02l
005600061127    2c                   enddo
005700061127   1bc                   endif
005800061127   1ac                   endif
005900061127     c
006000061127     c     kind03b       reade     cnind03l
006100061127    1c                   enddo
006200021212      *
006300030122     C                   SETON                                        LR
006400061127      *------------------------------------------------------------------------*
006500061127      * ROUTINE INIZIALE
006600061127      *------------------------------------------------------------------------*
006700061127     c     *Inzsr        BegSr
006800061127
006900061127     c     *dtaara       define    §azute        azuteds
007000061127     c     *dtaara       define    §datiute      ddatiute
007100061127     c                   in(E)     *dtaara
007200061127     c                   If        %error  or rsut = *blanks
007300061127     c                   Clear                   Tibs34ds
007400061127     c                   Call      'TIBS34R'
007500061127     c                   Parm                    Tibs34ds
007600061127     c                   In        *dtaara
007700061127     c                   EndIf
007800061127     c
007900061127     c     kind03a       klist
008000061127     c                   kfld                    kut
008100061127     c                   kfld                    dutkci
008200061127     c                   kfld                    kcdf
008300061127     c     kind03b       klist
008400061127     c                   kfld                    kut
008500061127     c                   kfld                    dutkci
008600061127     c     kind02a       klist
008700061127     c                   kfld                    kut
008800061127     c                   kfld                    dutkci
008900061127     c                   kfld                    kiva
009000061127     c     kaco          klist
009100061127     c                   kfld                    kut
009200061127     c                   kfld                    indkcc
009300061127     c                   kfld                    indksc
009400061127     c
009500061127     c                   ENDSR
