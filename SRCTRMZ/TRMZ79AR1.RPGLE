000100090109     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300111129     Faitrs01l  if   e           k disk
000301111129     Faitrsa1l  uf a e           k disk
000400090331      * ?___________________________________________________________________
000500111129     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0101R
000600090331     D prmRpyOpCode...
000700090331     D                 S             10A
000800090331     D prmRpyIdMsg...
000900090331     D                 S             10I 0
001000090331      * ?___________________________________________________________________
001100020304     dkpjba          e ds
001200111129     D trmz0101i     E DS                  prefix(x)
001300090408     D trmz70s1ds    E DS
001301111201     D trul28ds      E DS
001400090408     d paramI          ds
001500090430     D  parnrc                        7s 0
001600090430     D  pardat                        8s 0
001700090331      * ?___________________________________________________________________
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
001901111201     d cbarre11        s             11
001902111201     d cbarre12        s             12
002000000000     C*---------------------------------------------------------------*
002100090330      * ?______________  main   ____________________________________________
002200090330     c*
002201111129     c     ktrsa         klist
002202111129     c                   kfld                    trsnrc
002203111129     c                   kfld                    appen             1
002204111202     c                   move      '1'           appen
002300090430     c     parnrc        chain     aitrs01l
002400090430     c                   if        %found(aitrs01l)
002401111129     c     ktrsa         chain     aitrsa1l
002402111130     c                   if        %found(aitrsa1l)
002403111130     c                   z-add     wdatg         TRSADSC
002404111130     c                   update    aitrsa00
002405111130     c                   else
002406111130     c                   clear                   aitrsa00
002407111202     c                   move      '1'           trsacap
002408111130     c                   z-add     trsnrc        trsanrc
002409111202     c                   z-add     wdatg         TRSADEA
002410111130     c                   z-add     wdatg         TRSADSC
002411111130     c                   z-add     wdatg         TRSAunodc
002412111202     c                   z-add     wdatg         TRSAdfa
002413111130     c                   write     aitrsa00
002414111130     c                   endif
002500090430     c                   exsr      contratto
002600090430     c                   endif
002700090430      /FREE
002800111129       trmz0101r( 'FINALIZE'
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
004001111201      *imposta chk digit su barcode da stampare
004002111201     c                   clear                   cbarre12
004003111201     c                   exsr      srchkdgt
004004111201      *imposta parametri stampa
004100090330     c                   clear                   prmRpyOpCode
004200090330     c                   clear                   prmRpyIdMsg
004300111129     c                   clear                   trmz0101i
004400111130     c                   move      trsdec        dataiso
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
006401111130     c                   move      trsaunodc     dataiso
006402111130     c                   eval      xDATACERTAP = dataiso
006404111130     c                   eval      xDATAFIRMAV = dataiso
006406111130     c                   eval      xDATAdecapp = dataiso
006407111201     c                   eval      xcodbar12   = cbarre12
006500090401      /FREE
006600090401
006700111129       trmz0101r( 'PRTCONTRAT'
006800090401                : prmRpyOpCode
006900090401                : prmRpyIdMsg
007000111129                : 'TRMZ0101I'
007100111129                : trmz0101i
007200111129                : %SIZE(trmz0101i)
007300090401                );
007400090401
007500090401       IF prmRpyIdMsg < 0;
007600090401         // Stampa non riuscita.
007700090401       ENDIF;
007800090401
007900090401      /END-FREE
008501111129     c                   endif
008600090330     c                   endsr
008601111201      * ?___________________________________________________________________
008603111201     C* controllo check digits
008604111201      * ?___________________________________________________________________
008606111201     C     srchkdgt      BEGSR
008607111201      *
008608111201     c                   movel     trsnrc        cbarre11
008609111201     c                   movel     trsfil        comodo4           4
008610111202     c                   move      trsacap       comodo4
008611111201     c                   move      comodo4       cbarre11
008612111201     c                   movel(p)  cbarre11      cbarre12
008613111201     C                   clear                   trul28ds
008614111201     c                   eval      i28mod = 'BAR'
008615111201     c                   eval      i28cod = cbarre12
008616111201     c                   call      'TRUL28R1'
008617111201     c                   parm                    trul28ds
008618111201     c* se reperito barcode imposto i campi in stampa
008619111201     c                   if        o28err = *blanks
008620111201     c                   move      O28CKD        cbarre12
008621111201     c                   end
008622111201      *
008623111201     C                   ENDSR
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
012600111129       trmz0101r( 'INIT'
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
