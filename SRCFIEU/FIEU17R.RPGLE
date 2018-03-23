000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200140828     H* Fieu16R1 *---------------------------------------------------*
000300900418     H*                                                              *
000400080515     H* Scrittura SCANDATA  da aggiungere agli SCAN di delivery      *
000500140828     h* x segnalare che la Consegna è avvenuta con ISOLA FIN="I"     *
000600060811      **?___________________________________________________________ *
000700140829     ftivgd00f  if a E             DISK
000800080529      **?___________________________________________________________ *
000900140828     D S18record       S           2048
001000060810     d trul47ds      e ds
001100060810     D W0140           S             14  0 inz
001200060810      *
001300050920      **?__________________________________________________________________ */
001400050704      *    main
001500050920      **?__________________________________________________________________ */
001600140828     c     *entry        plist
001700140828     C                   parm                    ParcelNR         14
001800140828     C                   parm                    Depot             4
001900140828      ***
002000140828     C                   TIME                    ORADAT           14 0
002100140828     C                   MOVEL     ORADAT        ORATR             6 0
002200140828     C                   MOVEL     ORATR         ORATRa            6
002300140828     c                   move      *date         udtymd            8 0
002400140828     c                   move      udtymd        udtymda           8
002500140828     C                   TIME                    W0140
002600140828     C                   MOVE      W0140         W0080             8 0
002700140828      *
002800140828      *  istruzioni apertura blocco scrittura TIVGD
002900140828      *    fatto solo all'inizio del programma
003000140828     C                   clear                   trul47ds
003100140828     C                   eval      d47opz  = 'I'
003200140828     C                   eval      d47tip  = '$3'
003300140828     C                   eval      d47lck  = 'N'
003400140828     C                   eval      d47chkj = 'N'
003500140828     C                   eval      d47pgm  = 'FIEU16R1'
003600140828     C                   call      'TRUL47R'
003700140828     C                   parm                    trul47ds
003800140828      * Scrive SCAN 18
003900140828     c                   exsr      SCAN18
004000060810     C*
004100060810     C* Infine elimino il blocco elaborazione TIVGD
004200060810     C                   clear                   trul47ds
004300060810     C                   eval      d47opz  = 'F'
004400060810     C                   eval      d47tip  = '$3'
004500060810     C                   call      'TRUL47R'
004600060810     C                   parm                    trul47ds
004700050704      *
004800050705     c                   seton                                        lr
004900050920      **?__________________________________________________________________ */
005000140828     c     SCAN18        begsr
005100050920      **?__________________________________________________________________ */
005200140828      *
005300050920      *rekord testata scan
005400140828     c                   clear                   S18record
005500140828     c                   eval      S18record ='0818' +
005600061006     c                             udtymda + oratra +
005700140829     c                             '|D' + %trim(Depot) + '|T001|'
005800050920      *
005900060810     C                   exsr      tivgd
006000050920      *
006100140829      * es. di SCAN 18
006200140829      *  011820050601160506|D0170|T058|P01959990859540|YAL00|YADISLAND|YGI037|
006300140829      *
006400050920      * prepara scan per invio
006500140828     c                   clear                   S18record
006600140828     c                   eval      S18record ='0118' +
006700061006     c                             udtymda + oratra +
006800140828     c                             '|D' + %trim(Depot) + '|T001' +
006900140828     c                             '|P' + %trim(ParcelNR) +
007000140829     c                             '|YAL00|YADISLAND|YGI037|'
007100060810     C                   exsr      tivgd
007200060811      *
007300140828     c                   clear                   S18record
007400140828     c                   eval      S18record ='0918' +
007500061006     c                             udtymda + oratra +
007600140829     c                             '|D' + %trim(Depot) + '|T001|'
007700060810     C                   exsr      tivgd
007800060810
007900050920     c                   endsr
008000060810      **?__________________________________________________________________ */
008100060810      *   Scrittura tivgd file da trsmettere               *
008200060810      **?__________________________________________________________________ */
008300060810     C     tivgd         Begsr
008400060810      *
008500060810     c                   clear                   tivgd000
008600140828     c                   eval      vgddta = %TrimR(S18record)
008700060810     c                   eval      vgdtip = '$3'
008800060810     c                   eval      vgdksu = '0DPD0OUT'
008900060810     c                   eval      vgdtsc = 'WW'
009000140828     c                   eval      vgdpgm = 'FIEU16R1'
009100061023     c                   eval      vgddat = udtymd
009200060810      *
009300060810     C                   WRITE     tivgd000
009400060810      *
009500060810     C                   Endsr
009600060810      *----------------------------------------------------*
