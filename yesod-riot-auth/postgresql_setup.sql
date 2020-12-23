-- gen triggers - start




drop function public.process_audit_user() cascade;
create or replace function public.process_audit_user()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('user_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into user_history
                       (id, ident, password, email, is_admin, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.ident, new.password, new.email, new.is_admin, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into user_history
                       (id, ident, password, email, is_admin, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.ident, old.password, old.email, old.is_admin, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_user after insert or update or delete on public.user for each row execute procedure public.process_audit_user();



drop function public.process_audit_config() cascade;
create or replace function public.process_audit_config()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('config_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.code, new.string_value, new.int_value, new.double_value, new.bool_value, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.code, old.string_value, old.int_value, old.double_value, old.bool_value, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_config after insert or update or delete on public.config for each row execute procedure public.process_audit_config();



drop function public.process_audit_demoa() cascade;
create or replace function public.process_audit_demoa()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('demoa_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into demoa_history
                       (id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.myattr, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into demoa_history
                       (id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.myattr, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_demoa after insert or update or delete on public.demoa for each row execute procedure public.process_audit_demoa();



drop function public.process_audit_demob() cascade;
create or replace function public.process_audit_demob()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('demob_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into demob_history
                       (id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.myattr, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into demob_history
                       (id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.myattr, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_demob after insert or update or delete on public.demob for each row execute procedure public.process_audit_demob();



drop function public.process_audit_democ() cascade;
create or replace function public.process_audit_democ()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('democ_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into democ_history
                       (id, demob_id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.demob_id, new.myattr, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into democ_history
                       (id, demob_id, myattr, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.demob_id, old.myattr, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_democ after insert or update or delete on public.democ for each row execute procedure public.process_audit_democ();

-- gen triggers - end
