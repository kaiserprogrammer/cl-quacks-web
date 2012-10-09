set :user, "deploy"
set :runner, user
set :use_sudo, false

set :domain, "juergenbickert.de"
role :app, domain

set :application, "cl-quacks"
set :application_dir, "/var/www/apps/#{application}"

set :repository, "."

set :deploy_to, "#{application_dir}"
set :deploy_via, :copy
set :copy_exclude, [".git"]
set :scm, :git
set :branch, :master

namespace :deploy do

  desc <<-DESC
  A macro-task that updates the code and fixes the symlink.
  DESC
  task :default do
    transaction do
      update_code
      symlink
    end
  end

  task :update_code, :except => { :no_release => true } do
    on_rollback { run "rm -rf #{release_path}; true" }
    strategy.deploy!
  end

  task :after_deploy do
    cleanup
  end

  desc "Install quicklisp"
  task :install_quicklisp do
    run <<-CMD
    cd ~/
    if [ ! -d "quicklisp" ]; then
      curl -O http://beta.quicklisp.org/quicklisp.lisp;
      echo "" | sbcl --load quicklisp.lisp
           --eval "(quicklisp-quickstart:install)"
           --eval "(ql:add-to-init-file)"
           --eval "(quit)";
    fi
    cd -
CMD
  end

  desc "Install libraries"
  task :install_libraries do
    run <<-CMD
      sbcl
      --eval "(ql:quickload :)"
CMD
  end
end
