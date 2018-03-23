000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090330     Faitrs01l  uf   e           k disk
000400090331      * ?___________________________________________________________________
000500090331     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
000600090331     D prmRpyOpCode...
000700090331     D                 S             10A
000800090331     D prmRpyIdMsg...
000900090331     D                 S             10I 0
001000090331      * ?___________________________________________________________________
001100020304     dkpjba          e ds
001200090408     D trmz0100i     E DS                  prefix(x)
001300090408     D trmz70s1ds    E DS
001400090408     d paramI          ds
001500090430     D  parnrc                        7s 0
001600090430     D  pardat                        8s 0
001700090331      * ?___________________________________________________________________
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
002000000000     C*---------------------------------------------------------------*
002100090330      * ?______________  main   ____________________________________________
002200090330     c*
002300090430     c     parnrc        chain     aitrs01l
002400090430     c                   if        %found(aitrs01l)
002500090430     c                   exsr      contratto
002600090430     c                   endif
002700090430      /FREE
002800090430       trmz0100r( 'FINALIZE'
002900090430                : prmRpyOpCode
003000090430                : prmRpyIdMsg
003100090430                );
003200090430
003300090430      /END-FREE
003400090330     c                   seton                                        lr
003500090330      * ?___________________________________________________________________
003600090330     c     Contratto     begsr
003700090330      * ?___________________________________________________________________
003800090330     c* Richiamo PGM per stampa del contratto
003900090330     c                   exsr      RagSocCont
004000090330     c                   if        trovato = *on
004100090330     c                   clear                   prmRpyOpCode
004200090330     c                   clear                   prmRpyIdMsg
004300090330     c                   clear                   trmz0100i
004400090401     c                   move      trsdec        dataiso
004500090408     c                   eval      xCONTRATDAT  = dataiso
004600090408     c                   eval      xCONTRATNUM  = trsnrc
004700090408     c                   eval      xSOCIETA     = trssocg
004800090408     c                   eval      xFILIALE     = trsfil
004900090408     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
005000090407     c                                          %trim(ragsocksc1)
005100090408     c                   eval      xVETSLCOM    = %trim(localita)
005200090408     c                   eval      xVETSLIND    = %trim(indirizzo)
005300090408     c                   eval      xVETSLCAP    = %trim(cap)
005400090408     c                   eval      xVETSLPRV    = provincia
005500090408     c                   eval      xVETSLNAZ    = stato
005600090408     c                   eval      xVETCDFISCA  = CodiceFisc
005700090408     c                   eval      xVETCDIVA    = trsiva
005800090408     c                   eval      xVETRIMLUO   = %trim(trscit)
005900090408     c                   eval      xVETRIMNUM   = trsnis
006000090408     c                   eval      xVETLEGRAP   = %trim(trslra)
006100090408     c                   eval      xVETANAPRV   = trspia
006200090408     c                   eval      xVETANANUM   = trsnia
006300090424     c                   move      wdatg         dataiso
006400090424     c                   eval      xdatastampa = dataiso
006500090401      /FREE
006600090401
006700090401       trmz0100r( 'PRTCONTRAT'
006800090401                : prmRpyOpCode
006900090401                : prmRpyIdMsg
007000090401                : 'TRMZ0100I'
007100090401                : trmz0100i
007200090401                : %SIZE(trmz0100i)
007300090401                );
007400090401
007500090401       IF prmRpyIdMsg < 0;
007600090401         // Stampa non riuscita.
007700090401       ENDIF;
007800090401
007900090401      /END-FREE
008000090330      * se non c'è errore aggiorno data stampa contratto
008100090330     c                   if        prmrpyidmsg = 0
008200090330     c                   z-add     wdatg         trsdsc
008300090401     c                   update    aitrs000
008400090330     c                   endif
008500090430     c                   endif
008600090330     c                   endsr
008700090330      * ?___________________________________________________________________
008800090330     C     RagSocCont    BegSR
008900090330      * ?___________________________________________________________________
009000090330      *
009100090408     C                   clear                   Trmz70s1ds                     Output
009200090330     C                   movel(p)  TRSiva        PartitaIVA                     Input
009300090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
009400090330     C                   movel(p)  TRSFIL        Unita                          Input/Output
009500090430     C                   move      TrsSOCg       Societa                        Input/Output
009600090330      *
009700090330     c                   call      'TRMZ70SR1'
009800090408     C                   PARM                    trmz70s1ds
009900090330     c*
010000090330     C                   ENDSR
010100090330      * ?___________________________________________________________________
010200011026     c     *inzsr        begsr
010300090330      * ?___________________________________________________________________
010400020304     c     *entry        plist
010500020304     c                   parm                    kpjba
010600090408     c                   clear                   paramI
010700090408     c                   if        kpjbu <> *blank
010800090408     c                   movel     kpjbu         paramI
010900090408     c                   endif
011000020304
011100020218     C                   time                    w0120            14 0
011200090430     C                   move      w0120         wdat              8 0
011300020308      * data da impostare su data stampa e convalida se lancio = 'C'
011400020308     c                   move      wdat          dataeur
011500020308     c                   move      dataeur       dataiso
011600090331     c                   move      dataiso       wdatg
011700090424      * forzo la data di stampa se mi è stata passata (data certa)
011800090430     c                   if        pardat  > 0
011900090430     c                   z-add     pardat        wdatg             8 0
012000090424     c                   endif
012100090428      *
012200090424      *_______________________________________________________________
012300020313
012400090401      /FREE
012500090401
012600090401       trmz0100r( 'INIT'
012700090401                : prmRpyOpCode
012800090401                : prmRpyIdMsg
012900090401                );
013000090401
013100090401       IF prmRpyIdMsg < 0;
013200090401         RETURN;
013300090401       ENDIF;
013400090401
013500090401      /END-FREE
013600090401     c*
013700090401     c                   endsr
013800090330      * ?___________________________________________________________________
