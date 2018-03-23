000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300090223     H* TRTCT03R*----------------------------------------------------*
000400900418     H*                                                              *
000500090223     H* Ricevuto IFTMIN da Nilfisk  si deve dare l'info di ricevuto  *
000600090223     H*   con EDI si deve scrivere questo file per dire che il file  *
000700090223     H*   è arrivato .  (senza entrare nel merito degli errori)      *
000800090223     H*     .....al momento ......                                   *
000900060811      **?___________________________________________________________ *
001000080424     ftivgd00f  if a E             DISK    commit
001100080529      **?___________________________________________________________ *
001200060810
001300090223     D CTRLRecord      S           2048
001400060810     d trul47ds      e ds
001500060810     D W0140           S             14  0 inz
001600060810      *
001700900418     D KPJBA         E DS
001800060811     D Almeno_uno      S              1
001900050704
002000050705     D KPJBus          s                   like(kpjbu)
002100060811     D solo_inizio     s              1    inz(' ')
002200090223      **====================================================================*/
002300090303     D Segmento_UNA    C                   'UNA:+.?'
002400090303     D Segmento_UNB    C                   'UNB+UNOC:3+ITBAR005:ZZ'
002500090223     D Segmento_UNH    C                   'UNH+1+CONTRL:D:96A:UN:EAN002'
002600090223     D Segmento_UCI    C                   '+ITBAR005:ZZ+1'
002700090223     D Segmento_UNT    C                   'UNT+3+1'
002800090223     D Segmento_UNZ    C                   'UNZ+1+1'
002900090223      **?================================================================== */
003000090223      *    SCRITTURA DEL MESSAGGIO senza dettaglio errori
003100090223      **?================================================================== */
003200090223      * Schema messaggio:    UNA.....
003300090223      *                      UNB......
003400090223      *                      UNH......
003500090223      *                      UCI.....
003600090223      *                      UCM.....
003700090223      *                       "
003800090223      *                      UNT.....
003900090223      *                      UNZ.....
004000090223      **?================================================================== */
004100090223     c     *entry        plist
004200090226     C                   parm                    MITTENTE_UNB     35
004300090226     C                   parm                    MITTENTE_QUAL     4
004400090223     C                   parm                    ID_MESSAGGIO     14
004500090223      ***
004600090223
004700090223     C                   TIME                    ORADAT           14 0
004800090223     C                   MOVEL     ORADAT        ORATR             6 0
004900090223     C                   MOVEL     ORATR         ORAtra            4
005000090223     c                   move      *date         udtymd            8 0
005100090223     c                   move      udtymd        DATtra            6
005200090223     c                   move      udtymd        udtymda           8
005300090223     C                   TIME                    W0140
005400090223     C                   MOVE      W0140         W0080             8 0
005500090223      *----------------------------------------------------*
005600060811      **
005700090223      *  UNA......
005800090223     c                   clear                   CTRLRecord
005900090303     c                   eval      CTRLRecord = Segmento_UNA + ' '''
006000090223     C                   exsr      tivgd
006100090223      *
006200090223      *  UNB......
006300090223     c                   clear                   CTRLRecord
006400090223     c                   eval      CTRLRecord = Segmento_UNB    + '+' +
006500090226     c                                          %Trim(MITTENTE_UNB) + ':' +
006600090226     c                                          %Trim(MITTENTE_QUAL)+ '+' +
006700090223     c                                          datTRA + ':' +
006800090223     c                                          oraTRA + '+1'''
006900090223     C                   exsr      tivgd
007000090223      *
007100090223      *  UNH......
007200090223     c                   clear                   CTRLRecord
007300090223     c                   eval      CTRLRecord = Segmento_UNH + ''''
007400090223     C                   exsr      tivgd
007500090223      *
007600090223      *  UCI......
007700090223      *
007800090223     c                   clear                   CTRLRecord
007900090223     c                   eval      CTRLRecord = 'UCI+' +
008000090223     c                                          %Trim(ID_MESSAGGIO) + '+' +
008100090226     c                                          %Trim(MITTENTE_UNB) + ':' +
008200090226     c                                          %Trim(MITTENTE_QUAL)+
008300090223     c                                          Segmento_UCI + ''''
008400090223     C                   exsr      tivgd
008500090223      *
008600090223      *
008700090223      *  UNT......
008800090223     c                   clear                   CTRLRecord
008900090223     c                   eval      CTRLRecord = Segmento_UNT + ''''
009000090223     C                   exsr      tivgd
009100090223      *
009200090223      *
009300090223      *  UNZ......
009400090223     c                   clear                   CTRLRecord
009500090223     c                   eval      CTRLRecord = Segmento_UNZ + ''''
009600090223     C                   exsr      tivgd
009700090223      *
009800060810     C*
009900060810     C* Infine elimino il blocco elaborazione TIVGD
010000060811     c                   if        almeno_uno = 'S' and solo_inizio = 'S'
010100060810     C                   clear                   trul47ds
010200060810     C                   eval      d47opz  = 'F'
010300090224     C                   eval      d47tip  = 'EL'
010400060810     C                   call      'TRUL47R'
010500060810     C                   parm                    trul47ds
010600060811     c                   endif
010700050704      *
010800050705     c                   seton                                        lr
010900090223      **?__________________________________________________________________ */
011000090223      *   Scrittura tivgd file da trsmettere               *
011100090223      **?__________________________________________________________________ */
011200090223     C     tivgd         Begsr
011300090223      *
011400090223     c                   clear                   tivgd000
011500090223     c                   eval      vgddta = %TrimR(CTRLRecord)
011600090224     c                   eval      vgdtip = 'EL'
011700090224     c                   eval      vgdksu = '02494114'
011800090223     c                   eval      vgdtsc = 'WW'
011900090223     c                   eval      vgdpgm = 'TRTCT03R'
012000090223     c                   eval      vgddat = udtymd
012100090223      *
012200090223     C                   WRITE     tivgd000
012300090223      *
012400090223     C                   Endsr
012500050920      **?__________________________________________________________________ */
