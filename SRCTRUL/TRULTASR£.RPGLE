000100170511     H DECEDIT('0,') DATEDIT(*DMY.)
000200170511      * ==============================================================
000300170511      * ?TRULTAS - aggiorna TASLL2 flag ininfluente x svegliare i TRIGGER
000400170511      * ==============================================================
000500170511      *
000600170511      *  Con la spedizione aggiorna il TAS per attivare i TRIGGER e
000700170511      *  aggiornare il TITAS con le variazioni avvenute in FILIALE.
000800170511      *
000900170511      * ==============================================================
001000170511     fTITAS30C  uf   e           k disk    USROPN
001100170511      * ==============================================================
001200170511     d cmd             s             50    dim(01) ctdata perrcd(1)
001300170511     d cmdp            s             50    dim(01) ctdata perrcd(1)
001400170511     d comman          s             80
001500170511     d lenght          s             15  5 inz(80)
001600170511      *--
001700170511     d kpjba         e ds
001800170511      *--
001900170511     d trultasDS     e ds
002000170511      *------------------------------------------------------------------------*
002100170511      * per riconoscere da quale dei tre del combinato aggiornare
002200170511      *------------------------------------------------------------------------*
002300170511     ITITAS000      01
002400170511     ITITAS010      02
002500170511     ITITASP00      03
002600170511      * --------------------------------------------------------------
002700170511     c     *entry        plist
002800170511     c                   parm                    KPJBA
002900170511     c                   eval      trulTASds  = KPJBU
003000170511      **
003100170511     C                   SETON                                        LR
003200170511      **
003300170511     c     Bolla         klist
003400170511     c                   kfld                    kAAS
003500170511     c                   kfld                    kLNP
003600170511     c                   kfld                    kNRS
003700170511     c                   kfld                    kNSP
003800170511      **
003900170511     c                   move      TTASAAS       kAAS              4 0
004000170511     c                   move      TTASLNP       kLNP              3 0
004100170511     c                   move      TTASNRS       kNRS              2 0
004200170511     c                   move      TTASNSP       kNSP              7 0
004300170511      **
004400170511      * ?_____________________________________________________________      */
004500170511      * ?sistema informativo filiale reale(SETRAS)                          */
004600170511     c                   if        knsif = 'FILTRA201'
004700170511     c                   Movel(p)  cmd(1)        comman
004800170511     c                   else
004900170511      * ?sistema informativo filiale prova(AS888)                           */
005000170511     c                   Movel(p)  cmdp(1)       comman
005100170511     c                   endif
005200170511     c                   eval      lenght = %len(%trim(comman))
005300170511     c                   Call      'QCMDEXC'                            99
005400170511     c                   Parm                    comman
005500170511     c                   Parm                    lenght
005600170511     c  n99              Open      Titas30c
005700170511
005800170511     c     Bolla         chain     titas30C
005900170511      *------
006000170511     c                   if        %Found(titas30C)
006100170511      *------
006200170511     c                   if        tasLL2 = *blank
006300170511     c                   eval      tasLL2 = 'G'
006400170511     c                   else
006500170511     c                   eval      tasLL2 = *blank
006600170511     c                   end
006700170511      *------
006800170511     C   01              UPDATE    TITAS000
006900170511     C   02              UPDATE    TITAS010
007000170511     C   03              UPDATE    TITASP00
007100170511     c                   end
007200170511      **
007300170511     C                   CLOSE     TITAS30C
007400170511     c                   RETURN
007500170511      *---------------------------------------------------------
007600170511** cmd
007700170511OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
007800170511** cmdp
007900170511OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
