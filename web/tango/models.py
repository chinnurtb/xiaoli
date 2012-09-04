from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

from datetime import datetime

class Setting(db.Model):
    __tablename__ = 'settings'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)

    def __init__(self, name, value):
        self.name = name
        self.value = value

def lookup_profile(uid):
    data = {}
    profiles = Profile.query.filter_by(uid = uid).all()
    for p in profiles:
        data[p.key] = p.value
    return data

def update_profile(uid, key, value):
    profile = Profile.query.filter_by(uid=uid, key=key).first()
    if profile:
        profile.value = value
    else:
        profile = Profile(uid, key, value)
        db.session.add(profile)

def find_value(key, profiles):
    for p in profiles:
        if key == p.key:
            return p.value
    return None 

class Profile(db.Model):
    __tablename__ = 'profiles'
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    key = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)

    def __init__(self, uid, key, value):
        self.uid = uid
        self.key = key
        self.value = value

    @staticmethod
    def load(uid):
        profile = {}
        profiles = Profile.query.filter_by(uid = uid).all()
        for p in profiles:
            profile[p.key] = p.value
        return profile

        
    def update(self):
        profile = Profile.query.filter_by(user_id=self.user_id, key=self.key).first()
        if profile:
            profile.value = self.value
        else:
            profile = Profile(self.user_id, self.key, self.value)
            db.session.add(profile)
        db.session.commit()


class Query(db.Model):
    __tablename__ = 'queries'
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    tab = db.Column(db.String(100))
    mod = db.Column(db.String(100))
    name = db.Column(db.String(200))
    filters = db.Column(db.String)
    is_public = db.Column(db.Boolean)
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now) 

