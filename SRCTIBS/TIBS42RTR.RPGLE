000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201131001     fAZUTE01l  uf   e           k disk
000202131001     ftntbe01L  if   e           k disk
000203130930     D kpjba         e ds
000204131001     D dute01        e ds
000205131001     D daut          e ds
000400131001     D filprof         ds
000401131001     d  fil                           3
000402131001     d  prof                         10
000403131014     d const           s             29    inz('SNSSSSSSSNSSSNNSNSSSSSNSNSSSN')
001800000607      *--------------------------------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100131001     c                   parm                    kpjba
002101131001     c     ktab          klist
002102131001     c                   kfld                    codtab            3
002103131001     c                   move      'AUT'         codtab
002104131001     c
002105131001     c     ktab          setll     tntbe01l
002106131001     c                   do        *hival
002107131001     c     ktab          reade     tntbe01l
002108131001     c                   if        %eof(tntbe01l)
002109131001     c                   leave
002110131001     C                   endif
002111131001     c                   if        tbeatb = *blank
002112131001     c                   movel     tbeke1        filprof
002113131001     c                   movel(p)  tbeuni        daut
002114131001     c                   endif
002115131001     c     prof          chain     azute01l
002116131001     c                   if        %found(azute01l)
002117131001     c                   movel     utefaf        dute01
002121131021      *// -?testo se per il profilo è già stata impostata una definizione se si tengo buona la prima
002122131021      *// -?Se invece il profilo è EDP lo tratto successivamente in maniera forzata
002123131014     c                   if        §utedsaut <> *blank or
002124131021     c                             %subst(prof:1:3) = 'EDP'
002125131001     c                   iter
002126131001     c                   endif
002127131021      *// -?se in seconda chiave presente PO limito autorità a livello di filiale
002128131021      *// -?se blank abilito sull'area
002131131011     c                   select
002132131011     c                   when      fil = '046'
002133131011     c                   move      'AZ'          §uteautor
002134131011
002135131011     c                   when      %subst(prof:1:3) = 'RAM'
002136131011     c                   move      'AC'          §uteautor
002137131011      *
002138131014     c                   when      %subst(prof:1:3) = 'POC' or
002139131014     c                             %subst(prof:1:3) = 'REA'
002140131011     c                   move      'RA'          §uteautor
002141131011      *
002142131014     c                   when      %subst(prof:1:3) = 'DIV' or
002143131014     c                             %subst(prof:1:3) = 'RED'
002144131021     c                   move      'DI'          §uteautor
002145131011      *
002146131011     c                   other
002147131021      *// -?altri profili senza profilo in chiave non vengono riportati
002149131011     c                   if        tbeke2 =  *blank
002150131011     c                   iter
002151131011     c                   endif
002152131021      *// -?altri profili con seconda chiave diversa da PO profilo non vengono riportati
002154131011     c                   if        tbeke2 <> fil
002155131011     c                   iter
002156131001     c                   endif
002157131021      *// -?altri profili con filiale corrispondente al profilo autorità PO
002159131011     c                   move      'PO'          §uteautor
002160131011     c                   endsl
002161131011      *
002162131001     c                   movel     daut          §utedsaut
002163131002     c     ' ':'N'       xlate     §utedsaut     §utedsaut
002164131002     c                   move      *blank        §utetbeaut
002165131001     c                   movel     dute01        utefaf
002166131001     c                   update    azute000
002167131001     c                   endif
002168131001     C                   enddo
002169131003     c     *start        setll     azute01l
002170131003     c                   do        *hival
002171131003     c                   read      azute01l
002172131003     c                   if        %eof(azute01l)
002173131003     c                   leave
002174131003     c                   endif
002175131003     c                   movel     utefaf        dute01
002176131021      *// -?testo se per il profilo è già stata impostata una definizione se si tengo buona la prima
002177131003     c                   if        §uteautor <> *blank
002178131003     c                   iter
002179131003     c                   endif
002180131021     c                   if        %subst(uteute:1:3) = 'EDP'
002181131021      *// -?edp filiale autorizzazione area tranne flag sede
002182131021     c                   if        utefil <> 046
002183131014     c                   move      'RA'          §uteautor
002184131023     c                   move      *all'N'       §utedsaut
002185131023     c                   movel     const         §utedsaut
002186131014     c                   else
002187131021      *// -?edp sede solo in elenco autorizzati a tutto
002188131021     c                   if        uteute = 'EDPLB' or
002189131021     c                             uteute = 'EDPND' or
002190131021     c                             uteute = 'EDPAC' or
002191131021     c                             uteute = 'EDPBA' or
002192131021     c                             uteute = 'EDPMM' or
002193131021     c                             uteute = 'EDPAB' or
002194131022     c                             uteute = 'EDPLM' or
002195131021     c                             uteute = 'EDPMC' or
002196131021     c                             uteute = 'EDPRD' or
002197131021     c                             uteute = 'EDPCED'
002198131021     c                   move      'AZ'          §uteautor
002199131021     c                   move      *all'S'       §utedsaut
002200131021     c                   else
002201131021     c                   move      'NO'          §uteautor
002202131022     c                   move      *all'N'       §utedsaut
002203131021     c                   endif
002204131021     c                   endif
002205131021     c                   else
002206131021      *// -?altri profili non EDP                  ?
002207131003     c                   move      'NO'          §uteautor
002208131022     c                   move      *all'N'       §utedsaut
002209131014     c                   endif
002210131104     c                   move      *blank        §utetbeaut
002211131003     c                   movel     dute01        utefaf
002212131003     c                   update    azute000
002213131003     C                   enddo
002214131003      *
002804130930     c                   eval      *inLR = *on
