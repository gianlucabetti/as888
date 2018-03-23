000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300160610     ftitas40c  if   e           k disk
000400170614     ftitaa30c  if   e           k disk
000500170614     ftita430c  if   e           k disk
000600170614     ftabel00f  if   e           k disk
000700020828     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800170614     d ds15          E ds
000900170614     d stanaz          s                   like(tasnzm)
001000170614     d staloc          s             25
001100170614     d stap            s                   like(tascpd)
001200170614     d
001300000607      *--------------------------------------------------------------------
001400170614     C     KTAB          KLIST
001500170614     C                   KFLD                    CODUT             1 0
001600170614     C                   KFLD                    COD               2
001700170614     C                   KFLD                    KEY               8
001800170614     C     Kta4          KLIST
001900170614     C                   KFLD                    tasaas
002000170614     C                   KFLD                    taslnp
002100170614     C                   KFLD                    tasnrs
002200170614     C                   KFLD                    tasnsp
002300170614     C                   KFLD                    ktrc              1
002400170614     C     Ktaa          KLIST
002500170614     C                   KFLD                    tasaas
002600170614     C                   KFLD                    taslnp
002700170614     C                   KFLD                    tasnrs
002800170614     C                   KFLD                    tasnsp
002900170614     C                   KFLD                    ktrca             1
003000170614     C                   Z-ADD     1             CODUT
003100170614     C                   eval      cod='15'
003200170614     C                   eval      ktrc='P'
003300170614     C                   eval      ktrca='M'
003400170614     C                   clear                   key
003500170614     c                   except    testa
003600000608      * reperimento dati foglio
003700160610
003800160613     c     001           setll     titas40c
003900160613     c                   read      titas40c
004000160613    1c                   dow       not %eof(titas40c)
004100170614     c                   if        tasfiv>400
004200170614     c                   leave
004300170614     c                   endif
004400170614   2ac                   if        tasdft>20161231
004500170614     c     ktaa          chain     titaa30c
004600170614     c                   if        not %found(titaa30c)
004700170614     c                   clear                   taacpi
004800170614     c                   clear                   taaloc
004900170614     c                   endif
005000170614     c
005100160610
005200170614    3c                   if        tascpd<>*blanks or taacpi<>*blanks
005300170614     c
005400170614    4c                   if        taacpi<>*blanks and
005500170614     c                                            %subst(tastbl:1:1)='F'
005600170614     c* se si tratta di FRANCO verifico nazione con cod iso della p.iva
005700170614     c                   eval      key=tasnzm
005800170614     c     ktab          chain     tabel00f
005900170614    5c                   if        %found(tabel00f)
006000170614     c                   eval      ds15=tbluni
006100170614    6c                   if        §15ita<>'I' and §15iso<>%subst(taacpi:1:2)
006200170614     c                   eval      stanaz=tasnzm
006300170614     c                   eval      staloc=taaloc
006400170614     c                   eval      stap  =taacpi
006500170614     c                   except    errore
006600170614    6c                   endif
006700170614    6c                   if        §15ita='I' and §15iso<>%subst(taacpi:1:2)
006800170614     c                             and  %subst(taacpi:1:2)<'00'
006900170614     c* Se SM e cap inizia con 4789 --> ok
007000170614     c                   if        %subst(taacpi:1:2)<>'SM'  or
007100170614     c                             %subst(tascam:1:4)<>'4789'
007200170614     c                   eval      stanaz=tasnzm
007300170614     c                   eval      staloc=taaloc
007400170614     c                   eval      stap  =taacpi
007500170614     c
007600170614     c                   except    errore
007700170614    6c                   endif
007800170614    6c                   endif
007900170614    5c                   endif
008000170614     c
008100170614    4c                   endif
008200170614    4c                   if        %subst(tascpd:1:16)<>*blanks and
008300170614     c                                            %subst(tastbl:1:1)='A'
008400170614     c* se si tratta di FRANCO verifico nazione con cod iso della p.iva
008500170614     c                   eval      key=tasnzd
008600170614     c     ktab          chain     tabel00f
008700170614    5c                   if        %found(tabel00f)
008800170614     c                   eval      ds15=tbluni
008900170614    6c                   if        §15ita<>'I' and §15iso<>%subst(tascpd:1:2)
009000170614     c                   eval      stanaz=tasnzd
009100170614     c                   eval      staloc=taslod
009200170614     c                   eval      stap  =tascpd
009300170614     c                   except    errore
009400170614    6c                   endif
009500170614    6c                   if        §15ita='I' and §15iso<>%subst(tascpd:1:2)
009600170614     c                             and  %subst(tascpd:1:2)<'00'
009700170614     c                   if        %subst(tascpd:1:2)<>'SM'  or
009800170614     c                             %subst(tascad:1:4)<>'4789'
009900170614     c                   eval      stanaz=tasnzd
010000170614     c                   eval      staloc=taslod
010100170614     c                   eval      stap  =tascpd
010200170614     c                   except    errore
010300170614    6c                   endif
010400170614    6c                   endif
010500170614    5c                   endif
010600170614     c
010700170614    4c                   endif
010800160613    3c                   endif
010900160610     c
011000160613    2c                   endif
011100160610     c
011200160613     c                   read      titas40c
011300160610    1c                   enddo
011400160610     c
011500160610     c                   seton                                        lr
011600160610      /free
011700160610
011800170614     OQSYSPRT   E            testa          1
011900170614     O                                         +  0 'Fatture con cod ISO '
012000170614     O                                         +  0 'diverso da nazione'
012100170614     OQSYSPRT   E            errore         1
012200170614     o                                         +  0 'Sped:'
012300170614     O                       tasaas        z   +  1
012400170614     o                       taslnp        z   +  1
012500170614     o                       tasnrs        z   +  1
012600170614     o                       tasnsp        z   +  1
012700170614     o                       tastbl            +  1
012800170614     o                                         +  1 'Fatt:'
012900170614     o                       tasfiv        z   +  1
013000170614     o                       tasnft        z   +  1
013100170614     o                       tasdft            +  1 '    /  /  '
013200170614     o                                         +  1 'Loc/Naz/C.Iso:'
013300170614     o                       staloc            +  1
013400170614     o                       stanaz            +  1
013500170614     o                       §15iso            +  1
013600170614     o                                         +  1 'P.iva:'
013700170614     o                       stap              +  1
