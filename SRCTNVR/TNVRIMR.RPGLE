000100040721      *===============================================================*
000200050317      * TNSB16R    * Rigenerazione immagine LdV per clienti           *
000300040721      *===============================================================*
000400040721
000500040721     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040721
000700040721      *===============================================================*
000800040721
000900050617     fTITAS30C  uf   e           k disk
001000050617     fTILAC01L  if   e           k disk
001100040721
001200040721      *===============================================================*
001300040721
001400050318      * - Ds per campo TASFLO del file TITAS31C
001500040721     d dTASflo       e ds                  inz
001600040721      *
001700050617     d kaas            s                   like(tasaas)
001800050617     d klnp            s                   like(taslnp)
001900050617     d knrs            s                   like(tasnrs)
002000050617     d knsp            s                   like(tasnsp)
002100050318      * Predisposizione indicatori per riconoscere i tipi record
002200050617      *   reperiti dal file TITAS30C
002300050318     iTITAS000      31
002400050318     iTITAS010      32
002500050318     iTITASP00      33
002600040721
002700040721      *===============================================================*
002800040721      *  RIEPILOGO INDICATORI                                         *
002900040721      *---------------------------------------------------------------*
003000040721      *===============================================================*
003100040721
003200050617     C     ktas          klist
003300050617     C                   kfld                    kaas
003400050617     C                   kfld                    klnp
003500050617     C                   kfld                    knrs
003600050617     C                   kfld                    knsp
003700040721      *
003800050617     c     *loval        setll     tilac01l
003900050617    1c                   do        *hival
004000050617     c                   read      tilac01l
004100050617     c                   if        %eof(tilac01l)
004200050617     c                   leave
004300050617     c                   endif
004400050617    2c                   if        lacdir = 'CD                            '
004500050617     c                   clear                   kaas
004600050617     c                   clear                   klnp
004700050617     c                   clear                   knrs
004800050617     c                   clear                   knsp
004900050617     c                   move      lacaas        kaas
005000050617     c                   ADD       2000          kaas
005100050617     c                   move      laclnp        klnp
005200050617     c                   move      lacnrs        knrs
005300050617     c                   move      lacnsp        knsp
005400050617     c     ktas          setll     titas30c
005500050617    3c                   do        *hival
005600050617     c                   setoff                                       313233
005700050617     c     ktas          reade     titas30c
005800050617     c                   if        %eof(titas30c)
005900050617     c                   leave
006000050617     c                   endif
006100050617     c                   movel     tasflo        dtasflo
006200050617    4c                   if        §floiml = 'I'
006300050617     c                   eval      §floiml = 'S'
006400050617     c                   movel     dtasflo       tasflo
006500050617     c   31              UPDATE    TITAS000
006600050617     c   32              UPDATE    TITAS010
006700050617     c   33              UPDATE    TITASP00
006800050617    4c                   endif
006900050617    3c                   enddo
007000050617    2c                   endif
007100050617    1c                   enddo
007200040721      *
007300050617     c                   seton                                        lr
