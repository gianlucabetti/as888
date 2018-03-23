000001060512     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400060512      * chiude la spedizione aperta x solo incasso fatto da sede     *
000500960702     F*--------------------------------------------------------------*
000501080118     Fcnc0w13d  CF   E             WORKSTN
000700060512     FFNARB01L  UF   E           K DISK
001500030730      *---------------------------------------------------------------
001600030730     d* ds esterne
001700030730      *---------------------------------------------------------------
001800030730     D KPJBA         E DS
010901060505     d param           ds
010902080131     c* tipo elaborazione C=controlli/' '=Controllo + aggiornamento
010904060510     D kaas                           4s 0                                      SKI TAB. 3
010905060510     D klnp                           3s 0                                      SKI TAB. 3
010906060510     D knrs                           2s 0                                      SKI TAB. 3
010907060510     D knsp                           7s 0                                      SKI TAB. 3
010908080131     c* tipo incasso  C=C/ass/'A'=Assegnato
010909060519     D parca                          1                                         SKI TAB. 3
010910080131     c* esito ' ' = ok
010911060519     D paresito                       1                                         SKI TAB. 3
010912080131     D sifda                         10                                         SKI TAB. 3
010913080131     D sifa                          10                                         SKI TAB. 3
010914080131     D tipela                         1                                         SKI TAB. 3
011000911016      *---------------------------------------------------------------*
011100911016      * FLUSSO PRINCIPALE                                             *
011200911016      *---------------------------------------------------------------*
012600030730     c* controlli
012601060510     c                   clear                   paresito
012700030730     c                   exsr      srctr
012701060511     c                   if        *in90
012802080118     c* emetto la window per evidenziare meglio l'anomalia
012803080118     c                   do        *hival
012805080131     C                   EXFMT     LRZ1W01
012808080118     c   kl              leave
012809080118     c                   enddo
012810060511     c                   else
013400080131     c*aggiorna se richiesto
013401080131     c                   if        tipela = ' '
013600030730     c                   exsr      sraggio
013601080131     c                   end
013602060511     c                   end
013603080201     C                   write     frcdta
013604060511     c                   eval      kpjbu = param
014200941107     C*
014300030730     C                   SETON                                        LR
017000030730     c**********************************************************************
017100030730     C     srctr         BEGSR
017200030730     c**********************************************************************
017300030620     C                   SETOFF                                       90
017301080201     c                   do
026300060227     C     Kspe          CHAIN(n)  FNARB01L
026400060307     c                   if        not %found(fnarb01l)
026500060504     c                   seton                                        90
026501080118     c                   eval      wmsg= 'Non trovata spedizione'
026502130308     c                   eval      paresito = '2'
026503130312     c                   if        kcdaz = 'SC98'
026504130312     c                   setoff                                       90
026505130312     c                   end
026506080201     c                   leave
026700080201     c                   end
037100080118     c* Fattura aperto come ritorno all'incasso
037300060530     c                   if        arbicc<>'R' and parca='C'
037400060504     c                   seton                                        90
037402080201     c                   eval      wmsg= 'C/ass. non aperto come incasso'
037403130308     c                   eval      paresito = '1'
037404080201     c                   leave
037600080118     C                   END
037601060519     c* contrassegno aperto come ritorno all'incasso
037602060530     c                   if        arbica<>'R' and parca='A'
037603080201     c                   eval      wmsg= 'Fattura non aperta come incasso'
037604130308     c                   eval      paresito = '1'
037605060519     c                   seton                                        90
037606080201     c                   leave
037607060519     C                   END
037608060519     c* non esiste chiusura operatore
037609060519     c                   if        arbnmi = 0
037610060519     c                   seton                                        90
037611080118     c                   eval      wmsg= 'Manca il mandato d''introito'
037612130308     c                   eval      paresito = '1'
037613080201     c                   leave
037614060519     C                   END
0410370802011    c                   enddo
041100060224     C*
041200060224     C                   ENDSR
059933060412     C*-----------------------------------------------------*
059934060412     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
059935060412     C*-----------------------------------------------------*
059936060412     c     sraggio       begsr
059938060413     c* ERRATO INCASSO
059939060508     C     Kspe          CHAIN     FNARB01L
059940060508     c                   if        %found(fnarb01l)
0612000605303    c                   if        arbicc = 'R' and parca='C'
061300060512     c                   eval      arbicc = 'Y'
0614000604123    c                   end
0614010605303    c                   if        arbica = 'R' and parca='A'
061402060519     c                   eval      arbica = 'Y'
0614030605193    c                   end
061404080201     c                   eval      arbnmi = 969696
061500030730     C                   UPDATE    FNARB000
0615010605123    c                   end
066900030730     c*
067000030730     c                   endsr
067100940322     C*-----------------------------------------------------*
067200941107     C*    Operazioni iniziali
067300941107     C*-----------------------------------------------------*
067400940322     C     *INZSR        BEGSR
067500950907     C*
067600940322     C     *ENTRY        PLIST
067700940322     C                   PARM                    KPJBA
067701060505     c                   movel     kpjbu         param
067800020905     c*
069500940322      *---------------------------------------------------------------*
069600940322      * CHIAVI                                                        *
069700940322      *---------------------------------------------------------------*
071700060301     C     Kspe          KLIST
071800060301     C                   KFLD                    KAAS
071900060301     C                   KFLD                    KLNP
072000060301     C                   KFLD                    KNRS
072100060301     C                   KFLD                    KNSP
074700940322     C*
074800940322     C                   ENDSR
