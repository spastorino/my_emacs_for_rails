class ExampleController < ApplicationController

  def show
    redirect_to :controller => :units, :action => :fall
  end
  
  def write
    
  end
end
